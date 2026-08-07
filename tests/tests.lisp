(in-package #:sbcl-generations/tests)

(defun tests--store (root &rest arguments)
  "Return a generation store beneath ROOT."
  (apply #'make-generation-store
         :root (merge-pathnames "generations/" root)
         :current-pathname (merge-pathnames "current.sexp" root)
         :core-name "image.core"
         arguments))

(defun tests--publish-fake (store identifier &key (created-at 100)
                                                  (bytes (* 2 1024 1024))
                                                  metadata)
  "Write a complete ready generation directory without checkpointing.

The core is a placeholder, so this exercises the manifest, listing, selection,
and compatibility paths without spending a fork and a real image save."
  (let* ((directory (merge-pathnames (format nil "~A/" identifier)
                                     (sbcl-generations::generation-store-root store)))
         (core (merge-pathnames "image.core" directory))
         (manifest (merge-pathnames "manifest.sexp" directory)))
    (tests--write-core core bytes)
    (sbcl-generations::store--write-form
     manifest
     (append (list :generation
                   :version (sbcl-generations::generation-store-manifest-version
                             store)
                   :id identifier
                   :core (namestring core))
             metadata
             (list :sbcl-version (lisp-implementation-version)
                   :operating-system (software-type)
                   :operating-system-version (software-version)
                   :architecture (machine-type)
                   :created-at created-at)))
    manifest))

(defun tests--manifests (root)
  "Exercise manifest loading, listing, and compatibility."
  (let ((store (tests--store root)))
    (tests--publish-fake store "alpha" :created-at 100
                                       :metadata '(:source-commit "aaa"))
    (tests--publish-fake store "beta" :created-at 200)
    (let ((generations (generation-list store)))
      (test-assert (= (length generations) 2)
                   "every readable generation is listed")
      (test-assert (string= (generation-identifier (first generations)) "beta")
                   "generations are listed newest first")
      (test-assert (every #'generation-compatible-p generations)
                   "a generation saved by this runtime is compatible"))
    (test-assert (string= (generation-identifier (generation-find store "alpha"))
                          "alpha")
                 "a generation is found by identifier")
    (test-assert (null (generation-find store "missing"))
                 "an unknown identifier is not found")
    (test-assert (equal (getf (generation-metadata (generation-find store "alpha"))
                              :source-commit)
                        "aaa")
                 "host metadata survives the manifest round trip")
    (let ((incompatible (tests--store root)))
      (declare (ignore incompatible))
      (sbcl-generations::store--write-form
       (merge-pathnames "gamma/manifest.sexp"
                        (sbcl-generations::generation-store-root store))
       (list :generation :version 1 :id "gamma"
             :core (namestring
                    (merge-pathnames "gamma/image.core"
                                     (sbcl-generations::generation-store-root store)))
             :sbcl-version "0.0.0-not-this-runtime"
             :created-at 300))
      (let ((gamma (generation-find store "gamma")))
        (test-assert (not (null gamma))
                     "a generation with a foreign runtime still loads")
        (test-assert (not (generation-compatible-p gamma))
                     "a generation saved by another runtime is incompatible")
        (test-assert (eq (checkpoint-stage (lambda () (generation-select store gamma)))
                         :selection)
                     "an incompatible generation cannot be selected")))
    (test-assert (eq (checkpoint-stage
                      (lambda ()
                        (generation-load-manifest
                         (tests--write-core (merge-pathnames "bad.sexp" root) 16)
                         store)))
                     :manifest)
                 "a manifest that is not a generation record is refused")
    (let ((escaping (merge-pathnames "escaping/manifest.sexp"
                                     (sbcl-generations::generation-store-root store))))
      (sbcl-generations::store--write-form escaping
                                 (list :generation :version 1 :id "escaping"
                                       :core "/etc/passwd" :created-at 400))
      (test-assert (eq (checkpoint-stage
                        (lambda () (generation-load-manifest escaping store)))
                       :manifest)
                   "a core outside its artifact directory is refused")))
  nil)

(defun tests--selection (root)
  "Exercise the selection pointer and the rollback request."
  (let ((store (tests--store root)))
    (tests--publish-fake store "alpha" :created-at 100)
    (tests--publish-fake store "beta" :created-at 200)
    (test-assert (null (generation-selected store))
                 "no generation is selected before one is chosen")
    (generation-select store (generation-find store "alpha"))
    (test-assert (string= (generation-identifier (generation-selected store))
                          "alpha")
                 "the selected generation is reported back")
    (generation-select store (generation-find store "beta"))
    (test-assert (string= (generation-identifier (generation-selected store))
                          "beta")
                 "selecting again replaces the previous selection")
    (test-assert
     (handler-case
         (progn (generation-request-rollback store "alpha") nil)
       (rollback-requested (condition)
         (string= (sbcl-generations:rollback-requested-generation-identifier
                   condition)
                  "alpha")))
     "a rollback request signals and names its generation")
    (test-assert (string= (generation-identifier (generation-selected store))
                          "alpha")
                 "a rollback request selects durably before it signals")
    (test-assert (eq (checkpoint-stage
                      (lambda () (generation-request-rollback store "missing")))
                     :selection)
                 "rolling back to an unknown generation is refused")
    (let ((sbcl-generations:*checkpoint-in-progress-p* t))
      (test-assert (eq (checkpoint-stage
                        (lambda ()
                          (generation-select store (generation-find store "beta"))))
                       :selection)
                   "selection is refused while a checkpoint publishes")))
  nil)

(defun tests--backend-validation (root)
  "Exercise backend construction and publication refusals."
  (let ((store (tests--store root)))
    (test-assert (eq (checkpoint-stage
                      (lambda ()
                        (make-checkpoint-backend :toplevel-function
                                                 (lambda (arguments)
                                                   (declare (ignore arguments))
                                                   nil))))
                     :backend)
                 "a backend without a store is refused")
    (test-assert (eq (checkpoint-stage
                      (lambda () (make-checkpoint-backend :store store)))
                     :backend)
                 "a backend without a toplevel function is refused")
    (test-assert (eq (checkpoint-stage
                      (lambda ()
                        (make-generation-store :root (merge-pathnames "x/" root))))
                     :selection)
                 "a store without a pointer pathname is refused")
    (let ((generation (generation-find store "absent")))
      (test-assert (null generation)
                   "an empty store lists nothing"))
    (tests--publish-fake store "delta" :created-at 500)
    (let ((delta (generation-find store "delta")))
      (test-assert (eq (checkpoint-stage
                        (lambda () (generation-publish store delta)))
                       :publish)
                   "publishing without a saved core is refused")))
  nil)

(defun tests--checkpoint (root)
  "Exercise one real non-stopping checkpoint end to end.

This forks, saves a genuine image, boots it to confirm its identity, and
publishes it. It is the only test that proves the mechanism rather than the
bookkeeping around it."
  (let* ((store (tests--store root :manifest-version 7))
         (prepared nil)
         (resumed nil)
         (backend
           (make-checkpoint-backend
            :store store
            :toplevel-function (lambda (arguments)
                                 (declare (ignore arguments))
                                 nil)
            :identifier-function (lambda () "checkpointed")
            :precheck-function (lambda () :prechecked)
            :metadata-function (lambda (identifier precheck)
                                 (list :requested-by identifier
                                       :precheck precheck))
            :validate-function (lambda (precheck)
                                 (and (eq precheck :prechecked) :validated))
            :prepare-function (lambda (generation)
                                (declare (ignore generation))
                                (setf prepared t))
            :resume-function (lambda (state)
                               (setf resumed state))
            :probe-runner
            (sbcl-generations:make-sbcl-core-probe-runner
             :command (or (uiop:getenv "SBCL_GENERATIONS_SBCL") "sbcl")))))
    (test-assert (sbcl-generations::checkpoint--single-threaded-p)
                 "the test image is single threaded before checkpointing")
    (let ((generation (checkpoint-create backend)))
      (test-assert (eq (generation-status generation) :pending)
                   "a fresh checkpoint is pending while its coordinator runs")
      (test-assert (eq resumed :validated)
                   "the resume hook receives the validate hook's state")
      (test-assert (not prepared)
                   "the parent does not run the saver's prepare hook")
      (loop repeat 1200
            until (not (eq (generation-status generation) :pending))
            do (sleep 0.25))
      (test-assert (eq (generation-status generation) :ready)
                   (format nil "the checkpoint published, status ~S"
                           (generation-status generation)))
      (test-assert (probe-file (generation-core-pathname generation))
                   "the published core exists")
      (test-assert (not (probe-file
                         (sbcl-generations:generation-temporary-core-pathname
                          generation)))
                   "the unpublished core was renamed rather than copied")
      (test-assert (generation-compatible-p generation)
                   "the published core is compatible with this runtime")
      (let ((loaded (generation-find store "checkpointed")))
        (test-assert (not (null loaded))
                     "the published generation is listed")
        (test-assert (equal (getf (generation-metadata loaded) :requested-by)
                            "checkpointed")
                     "the metadata hook reached the manifest")
        (test-assert (eql (getf (generation-metadata loaded) :version) 7)
                     "the host manifest version was written")
        (test-assert (eq (getf (generation-metadata loaded) :precheck) :prechecked)
                     "the precheck value reaches the metadata hook"))
      (test-assert (string= (generation-identifier (generation-selected store))
                            "checkpointed")
                   "publication selects the generation it published")
      (test-assert (not sbcl-generations:*checkpoint-in-progress-p*)
                   "the watcher clears the in-progress flag")))
  nil)

(defun run-tests ()
  "Run every sbcl-generations regression test."
  (setf *test-count* 0)
  (let ((root (tests--temporary-root)))
    (unwind-protect
         (progn
           (tests--manifests (merge-pathnames "manifests/" root))
           (tests--selection (merge-pathnames "selection/" root))
           (tests--backend-validation (merge-pathnames "validation/" root))
           (tests--checkpoint (merge-pathnames "checkpoint/" root)))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  (format t "~&~:D sbcl-generations tests passed.~%" *test-count*)
  nil)
