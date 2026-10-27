(in-package #:sbcl-generations/tests)

(defun tests--source-command (root command &rest arguments)
  "Run a repository fixture command in ROOT and return its output."
  (string-right-trim
   '(#\Newline #\Return)
   (uiop:run-program (append (list command) arguments)
                     :directory root :output :string :error-output :string)))

(defun tests--source-write (root path contents)
  "Write CONTENTS as bytes or text to one fixture input."
  (let ((pathname (merge-pathnames path root)))
    (ensure-directories-exist pathname)
    (with-open-file (output pathname :direction :output :if-exists :supersede
                                    :element-type (if (stringp contents)
                                                      'character '(unsigned-byte 8)))
      (write-sequence contents output))))

(defun tests--source-commit (root kind)
  "Commit fixture inputs using KIND's normal workflow."
  (if (eq kind :git)
      (progn
        (tests--source-command root "git" "add" "--all")
        (tests--source-command root "git" "-c" "user.name=test" "-c"
                               "user.email=test@example.invalid" "commit" "-qm" "inputs"))
      (tests--source-command root "jj" "--config" "user.name='test'"
                             "--config" "user.email='test@example.invalid'"
                             "commit" "-m" "inputs")))

(defun tests--source-fixture (root kind)
  "Create a Git, standalone jj, or colocated jj source fixture."
  (ensure-directories-exist root)
  (ecase kind
    (:git
     (tests--source-command root "git" "init" "--quiet"))
    (:jj
     (tests--source-command root "jj" "git" "init" "--no-colocate"))
    (:colocated
     (tests--source-command root "jj" "git" "init" "--colocate")))
  (tests--source-write root "a.lisp" "(a)")
  (tests--source-write root "nested/b.lisp" "(b)")
  (tests--source-write root "literal(1).lisp" "(literal)")
  (tests--source-write root ".gitignore" "ignored.lisp
")
  (tests--source-commit root kind)
  root)

(defun tests--source-records (root kind)
  "Exercise selected-path provenance against actual Git and jj repositories."
  (let* ((tree     (tests--source-fixture root kind))
         (source   (sbcl-generations:make-source-provenance tree))
         (paths    '("a.lisp" "nested/b.lisp" "literal(1).lisp"))
         (inputs   (lambda (root) (declare (ignore root)) paths))
         (record   (image-build-record tree :inputs inputs))
         (revision (sbcl-generations:source-revision source)))
    (test-assert (eq (sbcl-generations:source-kind source)
                     (if (eq kind :git) :git :jj))
                 "the nearest backend is selected, preferring colocated jj")
    (test-assert (sbcl-generations:source-clean-p source :paths paths)
                 "committed source inputs are clean")
    (test-assert (string= revision
                         (if (eq kind :git)
                             (tests--source-command tree "git" "rev-parse" "HEAD")
                             (tests--source-command tree "jj" "log" "--no-graph"
                                                    "-r" "@-" "-T" "commit_id")))
                 "the revision identifies the baseline, not jj's working-copy snapshot")
    (test-assert (image-build-record-compatible-p record tree :inputs inputs)
                 "selected provenance produces compatible image records")
    (tests--source-write tree "unrelated.lisp" "(unrelated)")
    (test-assert (sbcl-generations:source-clean-p source :paths paths)
                 "unrelated changes do not dirty selected inputs")
    (test-assert (not (sbcl-generations:source-clean-p source))
                 "whole-tree cleanliness includes unrelated changes")
    (tests--source-write tree "a.lisp" "(edited)")
    (test-assert (not (sbcl-generations:source-clean-p source :paths '("a.lisp")))
                 "an edited requested input is dirty")
    (test-assert (not (image-build-record-compatible-p record tree :inputs inputs))
                 "edited input bytes invalidate a saved image")
    (tests--source-write tree "a.lisp" "(a)")
    (tests--source-write tree "literal(1).lisp" "(changed)")
    (test-assert (not (sbcl-generations:source-clean-p source
                                                    :paths '("literal(1).lisp")))
                 "fileset operators in requested names are literal")
    (tests--source-write tree "new.lisp" "(new)")
    (test-assert (not (sbcl-generations:source-clean-p source :paths '("new.lisp")))
                 "an untracked requested input is dirty")
    (unless (eq kind :git)
      (tests--source-write tree "ignored.lisp" "(ignored)")
      (test-assert (not (sbcl-generations:source-clean-p source :paths '("ignored.lisp")))
                   "jj snapshot exclusions cannot make a requested input clean"))
    (delete-file (merge-pathnames "nested/b.lisp" tree))
    (test-assert (not (sbcl-generations:source-clean-p source :paths '("nested/b.lisp")))
                 "a deleted requested input is dirty")
    (test-assert (eq :source
                     (tests--image-stage
                      (lambda () (sbcl-generations:source-file-identities
                                  source '("nested/b.lisp")))))
                 "missing input hashing signals a typed source failure")
    (let ((nested (sbcl-generations:make-source-provenance
                   (merge-pathnames "nested/" tree))))
      (test-assert (eq (sbcl-generations:source-kind nested)
                       (sbcl-generations:source-kind source))
                   "source roots inside a workspace inherit the nearest backend")
      (test-assert (string= revision (sbcl-generations:source-revision nested))
                   "nested source roots identify the same baseline"))
    (unless (eq kind :git)
      (let* ((inner        (tests--source-fixture (merge-pathnames "inner/" tree) :git))
             (inner-source (sbcl-generations:make-source-provenance inner)))
        (test-assert (eq :git (sbcl-generations:source-kind inner-source))
                     "a nested Git tree does not use outer jj provenance")))
    tree))

(defun tests--source-hashes (root)
  "Compare repository-free hashing with real Git on binary and boundary inputs."
  (ensure-directories-exist root)
  (let ((source (sbcl-generations:make-source-provenance root)))
    (test-assert (eq :directory (sbcl-generations:source-kind source))
                 "an unversioned directory has explicit provenance")
    (test-assert (and (string= "unversioned" (sbcl-generations:source-revision source))
                      (not (sbcl-generations:source-clean-p source)))
                 "unversioned sources have no committed baseline")
    (loop for size in '(0 1 65535 65536 65537 131073)
          for path = (format nil "bytes-~D" size)
          for bytes = (make-array size :element-type '(unsigned-byte 8))
          do (loop for index below size do (setf (aref bytes index) (mod index 256)))
             (tests--source-write root path bytes)
             (test-assert
              (string= (second (first (sbcl-generations:source-file-identities source (list path))))
                       (tests--source-command root "git" "hash-object" "--" path))
              "streaming hashes are byte-exact Git blob identities"))
    (let ((record (image-build-record root :inputs (lambda (root)
                                                    (declare (ignore root))
                                                    '("bytes-0" "bytes-65537")))))
      (test-assert (image-build-record-compatible-p
                    record root :inputs (lambda (root) (declare (ignore root))
                                          '("bytes-0" "bytes-65537")))
                   "unversioned source records match exact bytes"))))

(defun tests--source-bytes (pathname)
  "Read the exact bytes of one fixture file."
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence bytes stream)
      bytes)))

(defun tests--source-linked-git (root)
  "Exercise linked worktrees, shared object stores and stat-less Nix indexes."
  (let* ((tree   (tests--source-fixture (merge-pathnames "origin/" root) :git))
         (linked (merge-pathnames "linked/" root))
         (shared (merge-pathnames "shared/" root)))
    (tests--source-command tree "git" "worktree" "add" "--detach" (namestring linked))
    (tests--source-command tree "git" "clone" "--shared" (namestring tree) (namestring shared))
    (dolist (directory (list tree linked shared))
      (let ((source (sbcl-generations:make-source-provenance directory)))
        (test-assert (eq :git (sbcl-generations:source-kind source))
                     "gitdir files and shared object repositories use Git")
        (test-assert (and (sbcl-generations:source-clean-p source :paths '("a.lisp"))
                          (string= (sbcl-generations:source-revision source)
                                   (tests--source-command tree "git" "rev-parse" "HEAD")))
                     "linked and shared Git sources retain their committed identity")))
    (delete-file (merge-pathnames ".git/index" tree))
    (tests--source-command tree "git" "read-tree" "HEAD")
    (let ((index  (tests--source-bytes (merge-pathnames ".git/index" tree)))
          (source (sbcl-generations:make-source-provenance tree)))
      (test-assert (sbcl-generations:source-clean-p source :paths '("a.lisp"))
                   "a Nix-style stat-less index is clean")
      (test-assert (equalp index (tests--source-bytes (merge-pathnames ".git/index" tree)))
                   "provenance queries do not refresh the index"))))

(defun tests--source-jj-failure (root)
  "Refuse stale Git metadata when a colocated jj repository is broken."
  (let ((tree (tests--source-fixture root :colocated)))
    (rename-file (merge-pathnames ".jj/repo/store/type" tree)
                 (merge-pathnames ".jj/repo/store/type.saved" tree))
    (test-assert
     (eq :source (tests--image-stage
                  (lambda () (sbcl-generations:source-revision
                              (sbcl-generations:make-source-provenance tree)))))
     "a real jj failure signals instead of falling back to colocated Git")))

(defun tests--source-fresh-command (tree temporary)
  "Return a clean-process saver command using this library's registered sources."
  (let* ((script (make-pathname :type "lisp" :defaults temporary))
         (directories
           (remove-duplicates
            (loop for name in (asdf:registered-systems)
                  for system = (asdf:find-system name nil)
                  for file = (and system (asdf:system-source-file system))
                  when file collect (uiop:pathname-directory-pathname file))
            :test #'equal)))
    (with-open-file (output script :direction :output :if-exists :supersede)
      (dolist (form `((require :asdf)
                      (asdf:initialize-source-registry
                       '(:source-registry
                         ,@(mapcar (lambda (directory) `(:directory ,directory)) directories)
                         :ignore-inherited-configuration))
                      (asdf:load-system "sbcl-generations/tests")
                      (sbcl-generations:image-save
                       ,temporary
                       (sbcl-generations:image-build-record
                        ,tree :inputs #'sbcl-generations/tests::tests--image-inputs)
                       :inputs   #'sbcl-generations/tests::tests--image-inputs
                       :toplevel (lambda (arguments) (declare (ignore arguments))))))
        (let ((*package* (find-package :cl-user)) (*print-readably* t))
          (write form :stream output)
          (terpri output))))
    (list (namestring sb-ext:*runtime-pathname*) "--noinform" "--non-interactive"
          "--no-sysinit" "--no-userinit" "--load" (namestring script))))

(defun tests--source-fresh-image (root kind)
  "Build and probe an actual image from a clean saver process."
  (let* ((tree (tests--source-fixture (merge-pathnames "tree/" root) kind))
         (core (merge-pathnames "installed/image.core" root)))
    (tests--source-write tree "b.lisp" "(b)")
    (tests--source-commit tree kind)
    (image-install tree core :inputs      #'tests--image-inputs
                   :toplevel              (lambda (arguments) (declare (ignore arguments)))
                   :saver                 :fresh-process
                   :fresh-process-command (lambda (temporary)
                                            (tests--source-fresh-command tree temporary))
                   :probe-runner          (tests--image-runner))
    (test-assert (equal core (image-current-core core tree :inputs #'tests--image-inputs))
                 "a fresh saver embeds matching source provenance")
    (test-assert (search (generation-core-probe-output
                          (image-probe-record (image-installed-record core)))
                        (core-probe-run (tests--image-runner) core
                                        (list (namestring tree) *image-probe-argument*)))
                 "the saved core reselects its backend and answers its probe")
    (tests--source-write tree "a.lisp" "(edited)")
    (multiple-value-bind (output error-output status)
        (uiop:run-program
         (list (namestring sb-ext:*runtime-pathname*) "--core" (namestring core)
               (namestring tree) *image-probe-argument*)
         :output :string :error-output :string :ignore-error-status t)
      (declare (ignore output error-output))
      (test-assert (not (zerop status)) "the saved core refuses changed input bytes"))))

(defun tests--source-jj-merge (root)
  "Identify all jj parents and compare against their automatic merge."
  (let* ((tree    (tests--source-fixture (merge-pathnames "tree/" root) :jj))
         (base    (tests--source-command tree "jj" "log" "--no-graph" "-r" "@-"
                                         "-T" "commit_id"))
         (source  (sbcl-generations:make-source-provenance tree))
         (parents nil))
    (dolist (side '("left" "right"))
      (tests--source-command tree "jj" "new" base)
      (tests--source-write tree (format nil "~A.lisp" side) side)
      (tests--source-commit tree :jj)
      (push (tests--source-command tree "jj" "log" "--no-graph" "-r" "@-"
                                   "-T" "commit_id") parents))
    (apply #'tests--source-command tree "jj" "new" (reverse parents))
    (test-assert (string= (format nil "~{~A~^+~}" (reverse parents))
                         (sbcl-generations:source-revision source))
                 "a jj merge records each baseline parent in order")
    (test-assert (sbcl-generations:source-clean-p
                  source :paths '("a.lisp" "left.lisp" "right.lisp"))
                 "an empty jj merge compares cleanly against its merged parents")
    (let ((linked (merge-pathnames "workspace/" root)))
      (tests--source-command tree "jj" "workspace" "add" "--name" "linked"
                             (namestring linked))
      (let ((linked-source (sbcl-generations:make-source-provenance linked)))
        (test-assert (eq :jj (sbcl-generations:source-kind linked-source))
                     "a linked jj workspace selects jj provenance")
        (test-assert (sbcl-generations:source-clean-p linked-source :paths '("a.lisp"))
                     "a linked workspace queries its own working-copy state")))
    (setf parents nil)
    (dolist (side '("left" "right"))
      (tests--source-command tree "jj" "new" base)
      (tests--source-write tree "a.lisp" side)
      (tests--source-commit tree :jj)
      (push (tests--source-command tree "jj" "log" "--no-graph" "-r" "@-"
                                   "-T" "commit_id") parents))
    (apply #'tests--source-command tree "jj" "new" (reverse parents))
    (test-assert (not (sbcl-generations:source-clean-p source :paths '("a.lisp")))
                 "an inherited merge conflict is not a clean requested input")))

(defun tests--source-provenance (root)
  "Run repository, hashing, and clean-process provenance tests."
  (tests--source-records (merge-pathnames "git/" root) :git)
  (tests--source-hashes (merge-pathnames "unversioned/" root))
  (tests--source-linked-git (merge-pathnames "linked/" root))
  (tests--source-fresh-image (merge-pathnames "fresh-git/" root) :git)
  (if (handler-case (progn (uiop:run-program '("jj" "--version") :output :string) t)
        (error () nil))
      (progn
        (tests--source-records (merge-pathnames "jj/" root) :jj)
        (tests--source-records (merge-pathnames "colocated/" root) :colocated)
        (tests--source-jj-failure (merge-pathnames "broken-jj/" root))
        (tests--source-jj-merge (merge-pathnames "jj-merge/" root))
        (tests--source-fresh-image (merge-pathnames "fresh-jj/" root) :jj))
      (format t "~&Skipping Jujutsu integration tests: jj is not installed.~%")))
