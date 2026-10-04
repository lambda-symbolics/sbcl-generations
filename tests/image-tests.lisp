(in-package #:sbcl-generations/tests)

(defun tests--image-tree (root)
  "Create a Git work tree beneath ROOT holding two image inputs and return it."
  (let ((tree (merge-pathnames "tree/" root)))
    (ensure-directories-exist tree)
    (dolist (file '(("a.lisp" "(a)") ("b.lisp" "(b)")))
      (with-open-file (stream (merge-pathnames (first file) tree)
                              :direction :output :if-exists :supersede)
        (write-string (second file) stream)))
    (dolist (arguments '(("init" "--quiet")
                         ("add" "a.lisp" "b.lisp")
                         ("-c" "user.name=test" "-c" "user.email=test@example.invalid"
                          "commit" "--quiet" "-m" "inputs")))
      (uiop:run-program (append (list "git" "-C" (namestring tree)) arguments)))
    tree))

(defun tests--image-inputs (source-root)
  "Name the test image's inputs beneath SOURCE-ROOT."
  (declare (ignore source-root))
  (list "b.lisp" "a.lisp"))

(defun tests--image-runner ()
  "Return a probe runner booting cores with this test's own runtime."
  (make-sbcl-core-probe-runner
   :command (or (uiop:getenv "SBCL_GENERATIONS_SBCL")
                (namestring sb-ext:*runtime-pathname*))))

(defun tests--image-stage (thunk)
  "Return the stage of the CHECKPOINT-ERROR THUNK signals, or NIL."
  (handler-case (progn (funcall thunk) nil)
    (checkpoint-error (condition)
      (checkpoint-error-stage condition))))

(defun tests--image-records (root)
  "Exercise build records and their compatibility with a source tree."
  (let* ((tree (tests--image-tree root))
         (record (image-build-record tree :inputs #'tests--image-inputs)))
    (test-assert (image-build-record-p record)
                 "a build record is complete")
    (test-assert (equal (mapcar #'first (getf (rest record) :source-files))
                        '("a.lisp" "b.lisp"))
                 "inputs are sorted and identified by content")
    (test-assert (getf (rest record) :source-clean-p)
                 "a committed tree is clean")
    (test-assert (image-build-record-compatible-p record tree :inputs #'tests--image-inputs)
                 "a record matches the tree it describes")
    (with-open-file (stream (merge-pathnames "b.lisp" tree)
                            :direction :output :if-exists :supersede)
      (write-string "(changed)" stream))
    (test-assert (not (image-build-record-compatible-p record tree
                                                       :inputs #'tests--image-inputs))
                 "an edited input invalidates the record")
    (let ((properties (copy-list (rest record))))
      (setf (getf properties :sbcl-version) "0.0")
      (test-assert (not (image-build-record-compatible-p
                         (cons (first record) properties)
                         tree :inputs #'tests--image-inputs))
                   "a record from another runtime is incompatible"))
    (test-assert (null (image-current-core (merge-pathnames "missing.core" root) tree
                                           :inputs #'tests--image-inputs))
                 "a missing core is not current")
    (test-assert (eq :validation
                     (tests--image-stage
                      (lambda ()
                        (image-install tree (merge-pathnames "image.core" root)
                                       :inputs #'tests--image-inputs))))
                 "an install without a toplevel is refused")
    (test-assert (eq :save
                     (tests--image-stage
                      (lambda ()
                        (image-install tree (merge-pathnames "image.core" root)
                                       :inputs #'tests--image-inputs
                                       :toplevel (lambda (arguments) arguments)
                                       :saver :fresh-process
                                       :fresh-process-command
                                       (lambda (temporary)
                                         (declare (ignore temporary))
                                         (list (namestring sb-ext:*runtime-pathname*)
                                               "--non-interactive" "--no-sysinit"
                                               "--no-userinit" "--eval" "(sb-ext:exit :code 3)"))))))
                 "a failed fresh-process saver is reported"))
  nil)

(defun tests--image-install (root)
  "Exercise one real image save, probe, and install by fork."
  (let* ((tree (tests--image-tree root))
         (core (merge-pathnames "installed/image.core" root))
         (prepared nil))
    (test-assert (checkpoint-single-threaded-p)
                 "the test image is single threaded before saving an image")
    (image-install tree core
                   :inputs #'tests--image-inputs
                   :toplevel (lambda (arguments) arguments)
                   :prepare (lambda () (setf prepared t))
                   :probe-runner (tests--image-runner)
                   :probe-argument "--test-image-probe")
    (test-assert (not prepared)
                 "the parent does not run the saver's prepare hook")
    (test-assert (equal (image-current-core core tree :inputs #'tests--image-inputs) core)
                 "the installed image is current for its tree")
    (test-assert (equal (image-installed-record core)
                        (image-build-record tree :inputs #'tests--image-inputs))
                 "the manifest carries the build record")
    (test-assert (search (generation-core-probe-output
                          (image-probe-record (image-installed-record core)))
                         (core-probe-run (tests--image-runner) core
                                         (list (namestring tree) "--test-image-probe")))
                 "the installed core answers its probe")
    (with-open-file (stream (merge-pathnames "a.lisp" tree)
                            :direction :output :if-exists :supersede)
      (write-string "(edited)" stream))
    (test-assert (null (image-current-core core tree :inputs #'tests--image-inputs))
                 "an edited tree makes the installed image stale")
    (test-assert (eq :probe
                     (tests--image-stage
                      (lambda ()
                        (core-probe-run (tests--image-runner) core
                                        (list (namestring tree) "--test-image-probe")))))
                 "the core refuses to vouch for a tree it does not match"))
  nil)
