(in-package #:sbcl-generations)

;;;; -- Prebuilt Images --

;;; A prebuilt image is a core saved from a process that loaded a source tree,
;;; installed beside a manifest that names exactly which source files and which
;;; runtime it was built from. A host boots it instead of loading the tree when
;;; that record still matches, and the core proves it is the image the record
;;; describes by answering a probe before it is installed.

(defparameter *image-record-version* 1
  "The version of image build records and manifests.")

(defparameter *image-probe-argument*
  "--sbcl-generations-internal-image-probe"
  "The private argument an image answers with its build identity.

A probe passes the source root first and this argument second, so the image can
check its record against the tree before answering.")

(defvar *image-build-record* nil
  "The build record embedded in a saved image, or NIL in any other process.")

(defvar *image-inputs-function* nil
  "The function naming a saved image's source inputs, used to answer a probe.")

(defvar *image-toplevel-function* nil
  "The host entry point a saved image runs when it is booted normally.")

(defun image--git-output (source-root arguments)
  "Return trimmed output from Git ARGUMENTS run in SOURCE-ROOT."
  (handler-case
      (string-trim
       '(#\Space #\Tab #\Newline #\Return)
       (uiop:run-program (append (list "git" "-c" "safe.directory=*"
                                       "-C" (namestring source-root))
                                 arguments)
                         :output :string
                         :error-output :output))
    (error (condition)
      (generations--fail :source
                         (format nil "Could not identify the image source: ~A" condition)
                         :pathname source-root
                         :cause condition))))

(defun image--inputs (source-root inputs)
  "Return the sorted relative input pathnames INPUTS names beneath SOURCE-ROOT."
  (let ((pathnames (funcall inputs source-root)))
    (unless (and (consp pathnames)
                 (every (lambda (pathname) (and (stringp pathname) (plusp (length pathname))))
                        pathnames))
      (generations--fail :source "The image inputs must be a non-empty list of strings."
                         :pathname source-root))
    (sort (remove-duplicates (copy-list pathnames) :test #'string=) #'string<)))

(defun image--source-files (source-root pathnames)
  "Return (PATHNAME BLOB) pairs giving the Git content identity of PATHNAMES."
  (let ((blobs (uiop:split-string
                (image--git-output source-root (append '("hash-object" "--") pathnames))
                :separator '(#\Newline #\Return))))
    (unless (= (length blobs) (length pathnames))
      (generations--fail :source "Git returned the wrong number of source identities."
                         :pathname source-root))
    (mapcar #'list pathnames blobs)))

(defun image-build-record (source-root &key inputs)
  "Return the exact source and runtime identity of an image of SOURCE-ROOT.

INPUTS is a function of the source root returning the relative pathnames the
image is built from. Each is identified by its Git content hash, so the record
changes with any edit, committed or not."
  (let* ((source-root (uiop:ensure-directory-pathname source-root))
         (pathnames (image--inputs source-root inputs)))
    (append
     (list :sbcl-generations-image
           :version *image-record-version*
           :source-commit (image--git-output source-root '("rev-parse" "HEAD"))
           :source-clean-p (zerop (length (image--git-output
                                           source-root
                                           (append '("status" "--porcelain" "--")
                                                   pathnames))))
           :source-files (image--source-files source-root pathnames))
     (generation--runtime-properties))))

(defun image--source-files-p (value)
  "Return true when VALUE is a non-empty list of unique (PATHNAME BLOB) string pairs."
  (and (consp value)
       (handler-case (list-length value) (type-error () nil))
       (every (lambda (entry)
                (and (consp entry)
                     (eql (handler-case (list-length entry) (type-error () nil)) 2)
                     (stringp (first entry)) (plusp (length (first entry)))
                     (stringp (second entry)) (plusp (length (second entry)))))
              value)
       (= (length value) (length (remove-duplicates value :key #'first :test #'string=)))))

(defun image-build-record-p (value)
  "Return true when VALUE is a complete image build record."
  (not (null
        (and (consp value)
             (eq (first value) :sbcl-generations-image)
             (handler-case (evenp (list-length (rest value))) (type-error () nil))
             (eql (getf (rest value) :version) *image-record-version*)
             (let ((commit (getf (rest value) :source-commit)))
               (and (stringp commit) (plusp (length commit))))
             (typep (getf (rest value) :source-clean-p) 'boolean)
             (image--source-files-p (getf (rest value) :source-files))
             (loop for (key nil) on (generation--runtime-properties) by #'cddr
                   always (stringp (getf (rest value) key)))))))

(defun image-build-record-compatible-p (record source-root &key inputs)
  "Return true when RECORD exactly matches SOURCE-ROOT's inputs and this runtime."
  (not (null
        (handler-case
            (let ((source-root (uiop:ensure-directory-pathname source-root))
                  (files (and (image-build-record-p record)
                              (getf (rest record) :source-files))))
              (and files
                   (loop for (key value) on (generation--runtime-properties) by #'cddr
                         always (equal (getf (rest record) key) value))
                   (equal (mapcar #'first files) (image--inputs source-root inputs))
                   (equal files (image--source-files source-root (mapcar #'first files)))))
          (error ()
            nil)))))

(defun image-probe-record (record)
  "Return the exact identity an image built from RECORD reports when probed."
  (list :sbcl-generations-image-probe
        :version *image-record-version*
        :source-commit (getf (rest record) :source-commit)
        :source-files (getf (rest record) :source-files)))

(defun image-manifest-pathname (core-pathname)
  "Return the manifest pathname beside an installed image CORE-PATHNAME."
  (merge-pathnames "manifest.sexp" core-pathname))

(defun image-installed-record (core-pathname &key (read-function #'store--read-form))
  "Return the build record of the image installed at CORE-PATHNAME, or NIL.

The manifest is read with READ-FUNCTION, which takes a pathname and returns its
single form; NIL is returned for a missing or malformed manifest."
  (handler-case
      (let ((manifest (image-manifest-pathname core-pathname)))
        (when (probe-file manifest)
          (let ((form (funcall read-function manifest)))
            (when (and (consp form) (eq (first form) :sbcl-generations-image-manifest))
              (let ((record (getf (rest form) :record)))
                (and (image-build-record-p record) record))))))
    (error ()
      nil)))

(defun image-current-core (core-pathname source-root
                           &key inputs (read-function #'store--read-form))
  "Return CORE-PATHNAME when its installed image matches SOURCE-ROOT, else NIL.

The manifest carries the record the core embeds, so the comparison needs no boot."
  (and (probe-file core-pathname)
       (image-build-record-compatible-p
        (image-installed-record core-pathname :read-function read-function)
        source-root
        :inputs inputs)
       core-pathname))

(defun image-running-build-record ()
  "Return the build record embedded in this process's image, or NIL."
  *image-build-record*)

(defun image-save (pathname record &key inputs toplevel prepare)
  "Save this process as the image RECORD describes at PATHNAME and exit.

INPUTS answers later probes, TOPLEVEL receives the command-line arguments when
the image is booted normally, and PREPARE runs first so the host can clear
process-specific state. This never returns; a failure exits with status 1."
  (handler-case
      (progn
        (setf *image-build-record* record
              *image-inputs-function* inputs
              *image-toplevel-function* toplevel
              *checkpoint-in-progress-p* nil
              *checkpoint-core-probe-record* nil)
        (when prepare
          (funcall prepare))
        (sb-ext:gc :full t)
        (sb-ext:save-lisp-and-die (namestring pathname)
                                  :toplevel #'image--toplevel
                                  :executable nil
                                  :purify nil
                                  :compression nil))
    (error ()
      (sb-ext:exit :code 1 :abort t)))
  nil)

(defun image--toplevel ()
  "Answer a probe, or run the host entry point, in a booted image."
  (sb-ext:disable-debugger)
  (let ((arguments (uiop:command-line-arguments)))
    (if (and (= (length arguments) 2)
             (string= (second arguments) *image-probe-argument*))
        (if (image-build-record-compatible-p *image-build-record* (first arguments)
                                             :inputs *image-inputs-function*)
            (progn
              (write-string (generation-core-probe-output
                             (image-probe-record *image-build-record*))
                            *standard-output*)
              (finish-output *standard-output*))
            (progn
              (format *error-output* "This image does not match the source at ~A.~%"
                      (first arguments))
              (sb-ext:exit :code 1 :abort t)))
        (funcall *image-toplevel-function* arguments)))
  nil)

(defun image-install (source-root core-pathname
                      &key inputs toplevel prepare (saver :automatic)
                           fresh-process-command probe-runner
                           (publish-function #'image--publish-core)
                           (write-function #'store--write-form))
  "Build, probe, and atomically install an image of SOURCE-ROOT at CORE-PATHNAME.

SAVER :AUTOMATIC forks this process where the host can fork and only one Lisp
thread is alive; :FRESH-PROCESS, and every host without fork, runs the argv
FRESH-PROCESS-COMMAND returns for the temporary core pathname, whose process
must load the source and call IMAGE-SAVE. The saved core is booted through
PROBE-RUNNER and must report this record, the inputs must be unchanged
afterwards, and only then does PUBLISH-FUNCTION move the core into place and
WRITE-FUNCTION write its manifest. INPUTS, TOPLEVEL and PREPARE are as for
IMAGE-SAVE. Failures signal CHECKPOINT-ERROR with stages :SOURCE, :FORK, :SAVE,
:PROBE and :PUBLISH. Return CORE-PATHNAME."
  (let* ((source-root (uiop:ensure-directory-pathname source-root))
         (core-pathname (pathname core-pathname))
         (temporary (make-pathname :name (format nil ".~A.~D" (pathname-name core-pathname)
                                                 (sb-posix:getpid))
                                   :type "core"
                                   :defaults core-pathname))
         (record (image-build-record source-root :inputs inputs))
         (fork-p (and (eq saver :automatic) (image--fork-available-p))))
    (unless (functionp toplevel)
      (generations--fail :validation "An image needs a TOPLEVEL function."))
    (unless (member saver '(:automatic :fresh-process))
      (generations--fail :validation (format nil "Unknown image saver ~S." saver)))
    (unless (or fork-p fresh-process-command)
      (generations--fail :validation "This host needs a fresh-process image saver command."))
    (when (and fork-p (not (checkpoint-single-threaded-p)))
      (generations--fail :fork "Saving an image by fork requires one live Lisp thread."
                         :pathname core-pathname))
    (ensure-directories-exist core-pathname)
    (when (probe-file temporary)
      (delete-file temporary))
    (finish-output *standard-output*)
    (finish-output *error-output*)
    (unwind-protect
         (progn
           (unless (and (if fork-p
                            (image--fork-save (lambda ()
                                                (image-save temporary record
                                                            :inputs inputs
                                                            :toplevel toplevel
                                                            :prepare prepare))
                                              temporary)
                            (image--run-saver (funcall fresh-process-command temporary)
                                              temporary))
                        (probe-file temporary))
             (generations--fail :save "The image saver produced no core." :pathname temporary))
           (image--probe temporary source-root record
                         (or probe-runner (make-sbcl-core-probe-runner)))
           (unless (equal record (image-build-record source-root :inputs inputs))
             (generations--fail :source "The image inputs changed while it was built."
                                :pathname source-root))
           (handler-case
               (progn
                 (funcall publish-function temporary core-pathname)
                 (funcall write-function
                          (image-manifest-pathname core-pathname)
                          (list :sbcl-generations-image-manifest
                                :version *image-record-version*
                                :core (namestring core-pathname)
                                :built-at (get-universal-time)
                                :record record)))
             (error (condition)
               (generations--fail :publish
                                  (format nil "Could not install the image: ~A" condition)
                                  :pathname core-pathname
                                  :cause condition))))
      (when (probe-file temporary)
        (delete-file temporary)))
    core-pathname))

(defun image--publish-core (temporary core-pathname)
  "Move TEMPORARY over CORE-PATHNAME by rename."
  (uiop:rename-file-overwriting-target temporary core-pathname))

(defun image--run-saver (command temporary)
  "Run the fresh-process saver COMMAND and return true when it exited successfully."
  (handler-case
      (zerop (nth-value 2 (uiop:run-program command
                                            :input nil
                                            :output *standard-output*
                                            :error-output *error-output*
                                            :ignore-error-status t)))
    (error (condition)
      (generations--fail :save (format nil "Could not run the image saver: ~A" condition)
                         :pathname temporary
                         :cause condition))))

(defun image--probe (temporary source-root record runner)
  "Boot TEMPORARY through RUNNER and require RECORD's exact probe answer."
  (let ((actual (handler-case
                    (core-probe-run runner temporary
                                    (list (namestring source-root) *image-probe-argument*))
                  (checkpoint-error (condition)
                    (error condition))
                  (error (condition)
                    (generations--fail :probe "The image probe failed unexpectedly."
                                       :pathname temporary :cause condition)))))
    (unless (and (stringp actual)
                 (string= actual (generation-core-probe-output (image-probe-record record))))
      (generations--fail :probe "The saved image reported the wrong identity."
                         :pathname temporary)))
  nil)

(defun image--fork-available-p ()
  "Return true when this host can save an image from a forked child."
  #+win32 nil
  #-win32 t)

(defun image--fork-save (child-function temporary)
  "Run CHILD-FUNCTION in a forked child and return true when it exited successfully."
  #+win32
  (declare (ignore child-function))
  #+win32
  (generations--fail :fork "This host cannot fork an image saver." :pathname temporary)
  #-win32
  (let ((pid (handler-case (sb-posix:fork)
               (error (condition)
                 (generations--fail :fork
                                    (format nil "Could not fork the image saver: ~A" condition)
                                    :pathname temporary
                                    :cause condition)))))
    (if (zerop pid)
        (progn
          (funcall child-function)
          (sb-ext:exit :code 1 :abort t))
        (multiple-value-bind (waited status)
            (handler-case (sb-posix:waitpid pid 0)
              (error (condition)
                (generations--fail :save
                                   (format nil "Could not wait for the image saver: ~A"
                                           condition)
                                   :pathname temporary
                                   :cause condition)))
          (and (= waited pid)
               (sb-posix:wifexited status)
               (zerop (sb-posix:wexitstatus status))
               t)))))
