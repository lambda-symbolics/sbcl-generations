(in-package #:sbcl-generations)

;;;; -- Source Provenance --

(defclass source-provenance ()
  ((root :initarg :root :reader source-root
         :documentation "The source directory against which input paths are resolved."))
  (:documentation "A source tree's revision, cleanliness and byte identities."))

(defclass git-source-provenance (source-provenance) ()
  (:documentation "Source provenance from a Git working tree."))

(defclass jj-source-provenance (source-provenance) ()
  (:documentation "Source provenance from a Jujutsu workspace."))

(defclass directory-source-provenance (source-provenance) ()
  (:documentation "Byte identities for an unversioned source directory."))

(defgeneric source-kind (source)
  (:documentation "Return :GIT, :JJ or :DIRECTORY for SOURCE."))

(defgeneric source-revision (source)
  (:documentation "Return SOURCE's baseline revision string.

Git uses HEAD. Jujutsu uses the working-copy commit's parent commit IDs,
joined with + for a merge. Unversioned directories use unversioned."))

(defgeneric source-clean-p (source &key paths)
  (:documentation "Return exactly T when SOURCE has no changes in PATHS.

PATHS are literal file names relative to SOURCE-ROOT. NIL examines the whole
working tree. Requested untracked or deleted files count as changes.
Unversioned directories are never clean."))

(defgeneric source-file-identities (source paths)
  (:documentation "Return (PATH BLOB) pairs for the current bytes of PATHS.

PATHS are literal file names relative to SOURCE-ROOT. Git retains its repository's
object format; Jujutsu and unversioned directories use Git SHA1 blob identities.
Missing inputs signal CHECKPOINT-ERROR at stage :SOURCE."))

(defmethod source-kind ((source git-source-provenance)) :git)
(defmethod source-kind ((source jj-source-provenance)) :jj)
(defmethod source-kind ((source directory-source-provenance)) :directory)

(defun make-source-provenance (root)
  "Select provenance at the nearest managed ancestor of ROOT.

Prefer .jj over colocated .git. A nested Git tree takes precedence over an outer
Jujutsu workspace. Git directories and gitdir files are both recognized. Once
selected, a failed backend signals CHECKPOINT-ERROR rather than using another
backend's metadata. Selection is repeated in fresh savers and saved-core probes."
  (let ((root (uiop:ensure-directory-pathname (truename root))))
    (loop for directory = root then (uiop:pathname-parent-directory-pathname directory)
          while directory
          do (cond ((probe-file (merge-pathnames ".jj/" directory))
                    (return-from make-source-provenance
                      (make-instance 'jj-source-provenance :root root)))
                   ((probe-file (merge-pathnames ".git" directory))
                    (return-from make-source-provenance
                      (make-instance 'git-source-provenance :root root))))
          when (equal directory (uiop:pathname-parent-directory-pathname directory))
            do (return))
    (make-instance 'directory-source-provenance :root root)))

(defun source--command (source command arguments)
  "Run one provenance query, separating diagnostics from its result."
  (restart-case
      (handler-case
          (multiple-value-bind (output diagnostic status)
              (uiop:run-program (append command arguments)
                                :directory (source-root source)
                                :output :string :error-output :string
                                :ignore-error-status t)
            (unless (zerop status)
              (generations--fail
               :source (format nil "~A source query exited with status ~D: ~A"
                               (first command) status diagnostic)
               :pathname (source-root source)))
            (string-right-trim '(#\Newline #\Return) output))
        (checkpoint-error (condition)
          (error condition))
        (error (condition)
          (generations--fail :source
                             (format nil "Could not identify the image source: ~A" condition)
                             :pathname (source-root source) :cause condition)))
    (retry ()
      :report "Retry the source query after repairing the repository or executable."
      (source--command source command arguments))))

(defun source--git (source arguments)
  "Run a Git query without refreshing an immutable build tree's index."
  (source--command source
                   (list "git" "--no-optional-locks" "--literal-pathspecs" "-c" "safe.directory=*"
                         "-C" (namestring (source-root source)))
                   arguments))

(defun source--jj (source arguments)
  "Run a Jujutsu query, snapshotting current files before inspecting @."
  (source--command source '("jj" "--no-pager" "--color" "never") arguments))

(defmethod source-revision ((source git-source-provenance))
  (source--git source '("rev-parse" "HEAD")))

(defmethod source-revision ((source jj-source-provenance))
  (source--jj source '("log" "--no-graph" "-r" "@" "-T"
                       "parents.map(|parent| parent.commit_id()).join(\"+\")")))

(defmethod source-revision ((source directory-source-provenance))
  "unversioned")

(defmethod source-clean-p ((source git-source-provenance) &key paths)
  (zerop (length (source--git source
                             (append '("status" "--porcelain" "--untracked-files=all" "--")
                                     paths)))))

(defun source--jj-path (path)
  "Encode one literal current-directory file name as a Jujutsu fileset."
  (concatenate 'string "file:" (write-to-string path :escape t)))

(defmethod source-clean-p ((source jj-source-provenance) &key paths)
  (let ((filesets (mapcar #'source--jj-path (remove-duplicates paths :test #'string=))))
    (and (zerop (length (source--jj source
                                   (append '("diff" "-r" "@" "--name-only" "--")
                                           filesets))))
         (let ((entries (source--jj
                         source (append '("file" "list" "-r" "@" "-T"
                                          "if(conflict, \"!\", \"t\")" "--")
                                        filesets))))
           ;; Snapshot policies can leave requested new files untracked.
           (and (not (find #\! entries))
                (or (null paths) (= (length entries) (length filesets))))))))

(defmethod source-clean-p ((source directory-source-provenance) &key paths)
  (declare (ignore paths))
  nil)

(defun source--blob (pathname)
  "Hash PATHNAME's bytes as a Git SHA1 blob without a repository process."
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (let ((digest (ironclad:make-digest :sha1))
          (buffer (make-array 65536 :element-type '(unsigned-byte 8))))
      (ironclad:update-digest
       digest (map '(vector (unsigned-byte 8)) #'char-code
                   (format nil "blob ~D~C" (file-length stream) #\Null)))
      (loop for count = (read-sequence buffer stream)
            while (plusp count)
            do (ironclad:update-digest digest buffer :end count))
      (ironclad:byte-array-to-hex-string (ironclad:produce-digest digest)))))

(defmethod source-file-identities ((source source-provenance) paths)
  (handler-case
      (mapcar (lambda (path)
                (list path (source--blob (merge-pathnames path (source-root source)))))
              paths)
    (error (condition)
      (generations--fail :source "Could not hash an image input."
                         :pathname (source-root source) :cause condition))))

(defmethod source-file-identities ((source git-source-provenance) paths)
  (when paths
    (let ((blobs (uiop:split-string
                  (source--git source (append '("hash-object" "--") paths))
                  :separator '(#\Newline #\Return))))
      (unless (= (length blobs) (length paths))
        (generations--fail :source "Git returned the wrong number of source identities."
                           :pathname (source-root source)))
      (mapcar #'list paths blobs))))
