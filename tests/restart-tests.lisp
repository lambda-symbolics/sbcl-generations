(in-package #:sbcl-generations/tests)

(defvar *restart-test-heap* nil
  "Cyclic heap state that cannot be recreated from a source replay.")

(defvar *restart-test-secret* nil
  "Fixture resource that must be cleared before saving.")

(defun tests--restart-child (root mode)
  "Save an exact heap, or exercise the requested failure MODE in a child."
  (let* ((store (tests--store root))
         (revision 1)
         (backend
           (sbcl-generations:make-restart-checkpoint-backend
            :store store
            :identifier-function (lambda () "restart")
            :toplevel-function
            (lambda (arguments)
              (assert (equal arguments '("resumed-session")))
              (assert (eq (rest *restart-test-heap*) *restart-test-heap*))
              (assert (= (first *restart-test-heap*) 42))
              (assert (null *restart-test-secret*))
              (format t "exact-heap-resumed~%"))
            :precheck-function (lambda () revision)
            :metadata-function (lambda (identifier value)
                                 (declare (ignore identifier))
                                 (list :revision value :session "resumed-session"))
            :validate-function
            (lambda (value)
              (assert (= value 2))
              (when (eq mode :validate-failure)
                (error "validation fixture"))
              :quiesced)
            :prepare-function
            (lambda (generation)
              (declare (ignore generation))
              (setf *restart-test-secret* nil)
              (when (eq mode :prepare-failure)
                (error "prepare fixture")))
            :resume-function
            (lambda (state)
              (assert (eq state :quiesced))
              (sbcl-generations::store--write-form
               (merge-pathnames "resumed.sexp" root) '(:resumed))))))
    (setf *restart-test-secret* "fixture"
          *restart-test-heap* (list 1))
    (setf (rest *restart-test-heap*) *restart-test-heap*)
    (let ((generation (sbcl-generations:checkpoint-restart-create backend)))
      ;; Mutate after reservation to prove saving occurs at the safe boundary.
      (setf revision 2 (first *restart-test-heap*) 42)
      (sbcl-generations:checkpoint-restart-save-and-exit
       backend generation
       :ready-function
       (lambda (ready)
         (sbcl-generations::store--write-form
          (merge-pathnames "request.sexp" root)
          (list :metadata (generation-metadata ready))))))))

(defun tests--restart-run-child (root mode)
  "Launch a clean child loaded from the same ASDF source directories."
  (let* ((script (merge-pathnames "save.lisp" root))
         (directories
           (remove-duplicates
            (loop for name in (asdf:registered-systems)
                  for system = (asdf:find-system name nil)
                  for file = (and system (asdf:system-source-file system))
                  when file collect (uiop:pathname-directory-pathname file))
            :test #'equal)))
    (ensure-directories-exist script)
    (with-open-file (output script :direction :output :if-exists :supersede)
      (dolist (form
               `((require :asdf)
                 (asdf:initialize-source-registry
                  '(:source-registry
                    ,@(mapcar (lambda (directory) `(:directory ,directory))
                              directories)
                    :inherit-configuration))
                 (asdf:load-system "sbcl-generations/tests")
                 (tests--restart-child ,root ',mode)))
        (let ((*package* (find-package :cl-user)) (*print-readably* t))
          (write form :stream output)
          (terpri output))))
    (uiop:run-program
     (list (namestring sb-ext:*runtime-pathname*)
           "--noinform" "--non-interactive" "--load" (namestring script))
     :output :string :error-output :output :ignore-error-status t)))

(defun tests--restart-reservation (root)
  "Check reservation, hook timing, and duplicate request rejection."
  (let* ((validated nil)
         (backend
           (sbcl-generations:make-restart-checkpoint-backend
            :store (tests--store root)
            :toplevel-function (lambda (arguments) (declare (ignore arguments)))
            :validate-function (lambda (value)
                                 (declare (ignore value)) (setf validated t)))))
    (test-assert (eq (generation-status (checkpoint-create backend)) :pending)
                 "reservation returns a pending generation")
    (test-assert (not validated) "reservation does not quiesce the application")
    (test-assert (eq (checkpoint-stage (lambda () (checkpoint-create backend)))
                     :validation)
                 "duplicate reservations are refused")))

(defun tests--restart-request-validation (root)
  "Refuse invalid supervisor identities before they can name artifacts."
  (let ((store (tests--store root)))
    (dolist (identifier '("" "." ".." "../escape" "C:\\escape" "wild*"))
      (test-assert
       (eq (checkpoint-stage
            (lambda ()
              (sbcl-generations:generation-recreate-pending
               store :identifier identifier :created-at 123 :metadata nil)))
           :manifest)
       "unsafe request identifiers are refused"))))
(defun tests--restart-checkpoint (root)
  "Save, probe, publish and resume a real heap; then exercise failed saves."
  (tests--restart-reservation (merge-pathnames "reservation/" root))
  (tests--restart-request-validation (merge-pathnames "request-validation/" root))
  (dolist (mode '(:success :validate-failure :prepare-failure))
    (let* ((directory (merge-pathnames (format nil "~(~A~)/" mode) root))
           (store (tests--store directory)))
      (multiple-value-bind (output errors code)
          (tests--restart-run-child directory mode)
        (declare (ignore errors))
        (test-assert (= code (if (eq mode :success) 0 1))
                     (format nil "restart child ~A exit ~D: ~A" mode code output)))
      (let ((generation
              (sbcl-generations:generation-recreate-pending
               store :identifier "restart" :created-at 123
               :metadata '(:revision 2 :session "resumed-session"))))
        (if (eq mode :success)
            (progn
              (test-assert
               (equal (sbcl-generations::store--read-form
                       (merge-pathnames "request.sexp" directory))
                      '(:metadata (:revision 2 :session "resumed-session")))
               "supervisor receives final refreshed metadata")
              (sbcl-generations:generation-publish
               store generation
               :probe-runner (sbcl-generations:make-sbcl-core-probe-runner
                              :command (namestring sb-ext:*runtime-pathname*)))
              (test-assert (string= (generation-identifier (generation-selected store))
                                   "restart")
                           "verified exact core is selected")
              (test-assert
               (search "exact-heap-resumed"
                       (uiop:run-program
                        (list (namestring sb-ext:*runtime-pathname*) "--noinform"
                              "--core" (namestring (generation-core-pathname generation))
                              "--end-runtime-options" "resumed-session")
                        :output :string :error-output :output))
               "cyclic heap, post-reservation mutation and session survive restart")
              (test-assert (not (probe-file (merge-pathnames "resumed.sexp" directory)))
                           "successful saving does not invoke the resume hook"))
            (progn
              (test-assert
               (probe-file (merge-pathnames "failure.sexp"
                                           (sbcl-generations:generation-directory generation)))
               "failed checkpoint records a failure artifact")
              (test-assert (null (generation-selected store))
                           "failed checkpoint is never selected")
              (test-assert
               (eq (not (null (probe-file (merge-pathnames "resumed.sexp" directory))))
                   (eq mode :prepare-failure))
               "only completed validation is resumed after failure")))))))
