(in-package #:sbcl-generations)

;;;; -- Saved Core Entry Point --

(defvar *checkpoint-core-probe-record* nil
  "The identity embedded in a saved core, printed when it is probed.")

(defvar *checkpoint-core-probe-argument* nil
  "The private argument the saved core recognizes as a probe request.")

(defvar *checkpoint-core-toplevel-function* nil
  "The host entry point a saved core runs when it is booted normally.")

(defun checkpoint-resume-toplevel ()
  "Answer a probe, or run the host entry point, in a booted saved core.

This is the toplevel of every core this library saves. A core has to be able to
prove which generation it is before publication trusts it, and it can only do
that by being booted, so the probe answer lives in the same entry point as
ordinary startup."
  (sb-ext:disable-debugger)
  (let ((arguments (uiop:command-line-arguments)))
    (if (and *checkpoint-core-probe-argument*
             (equal arguments (list *checkpoint-core-probe-argument*)))
        (progn
          (write-string (generation-core-probe-output *checkpoint-core-probe-record*)
                        *standard-output*)
          (finish-output *standard-output*))
        (restart-case
            (funcall *checkpoint-core-toplevel-function* arguments)
          (abort ()
            :report "Exit this saved core."
            nil))))
  nil)


;;;; -- Checkpoint Backend --

(defclass checkpoint-backend ()
  ((store
    :initarg :store
    :reader checkpoint-backend-store
    :type generation-store
    :documentation "Where this backend publishes the images it saves.")
   (toplevel-function
    :initarg :toplevel-function
    :reader checkpoint-backend-toplevel-function
    :type function
    :documentation "The host entry point a saved core runs, called with arguments.")
   (identifier-function
    :initarg :identifier-function
    :reader checkpoint-backend-identifier-function
    :type function
    :documentation "Returns a fresh identifier naming the next generation.")
   (precheck-function
    :initarg :precheck-function
    :reader checkpoint-backend-precheck-function
    :type (or null function)
    :documentation "Runs slow validation before anything is exclusive, or NIL.")
   (metadata-function
    :initarg :metadata-function
    :reader checkpoint-backend-metadata-function
    :type (or null function)
    :documentation "Returns the manifest properties inside the exclusive region.")
   (validate-function
    :initarg :validate-function
    :reader checkpoint-backend-validate-function
    :type (or null function)
    :documentation "Checks the process just before the fork and returns state.")
   (prepare-function
    :initarg :prepare-function
    :reader checkpoint-backend-prepare-function
    :type (or null function)
    :documentation "Detaches host resources inside the saver child, or NIL.")
   (resume-function
    :initarg :resume-function
    :reader checkpoint-backend-resume-function
    :type (or null function)
    :documentation "Restores whatever VALIDATE-FUNCTION suspended, or NIL.")
   (around-function
    :initarg :around-function
    :reader checkpoint-backend-around-function
    :type (or null function)
    :documentation "Wraps the whole checkpoint in a host dynamic context, or NIL.")
   (fork-guard-function
    :initarg :fork-guard-function
    :reader checkpoint-backend-fork-guard-function
    :type (or null function)
    :documentation "Wraps the pre-fork checks and the fork itself, or NIL.")
   (probe-runner
    :initarg :probe-runner
    :reader checkpoint-backend-probe-runner
    :type (or null generation-core-probe-runner)
    :documentation "Boots the unpublished core to confirm its identity."))
  (:documentation "The platform boundary for one non-stopping image checkpoint."))

(defclass fork-checkpoint-backend (checkpoint-backend)
  ()
  (:documentation "A checkpoint implemented with a coordinator and saver fork."))

(defun checkpoint--default-identifier ()
  "Return a plain identifier for a host that does not supply its own."
  (format nil "~36R-~36R" (get-universal-time) (random most-positive-fixnum)))

(defun make-checkpoint-backend
    (&key
       store
       toplevel-function
       (identifier-function #'checkpoint--default-identifier)
       precheck-function
       metadata-function
       validate-function
       prepare-function
       resume-function
       around-function
       fork-guard-function
       probe-runner)
  "Create the checkpoint backend this runtime supports.

STORE and TOPLEVEL-FUNCTION are required. TOPLEVEL-FUNCTION is called with the
saved core's command-line arguments whenever that core is booted normally.

The hooks run at these points, and each may be omitted:

- AROUND-FUNCTION receives a thunk and wraps the entire checkpoint. Use it to
  hold whatever dynamic context the rest of the hooks assume.
- PRECHECK-FUNCTION runs before anything is exclusive and returns a value passed
  on to the two hooks below. Slow work such as validating a source tree belongs
  here, precisely because nothing is being held while it runs.
- FORK-GUARD-FUNCTION receives a thunk and wraps everything that follows,
  together with the fork itself. That is the only region that must be exclusive,
  and it is kept as short as the work allows.
- VALIDATE-FUNCTION receives PRECHECK-FUNCTION's value inside that region and
  returns state. Re-check anything the precheck established, since time has
  passed and this is the last chance. Whatever it returns is handed to
  RESUME-FUNCTION.
- METADATA-FUNCTION receives the new identifier and PRECHECK-FUNCTION's value,
  and returns the manifest properties. It runs inside the exclusive region, so
  a host may record state that must not change between here and the fork.
- PREPARE-FUNCTION receives the generation inside the saver child, before the
  image is written. Detach inherited descriptors and clear secrets here: they
  would otherwise be saved into the core.
- RESUME-FUNCTION receives VALIDATE-FUNCTION's state in the parent, whether the
  fork succeeded or not.

Checkpointing needs SBCL's fork, so a runtime without it is refused here rather
than at the fork."
  (unless (typep store 'generation-store)
    (generations--fail :backend "A checkpoint backend needs a generation store."))
  (unless (functionp toplevel-function)
    (generations--fail :backend "A checkpoint backend needs a toplevel function."))
  (unless (and (member :sbcl *features*)
               (or (member :linux *features*)
                   (member :darwin *features*)))
    (generations--fail :backend
                       "Non-stopping checkpoints require SBCL on Linux or macOS."))
  (make-instance 'fork-checkpoint-backend
                 :store store
                 :toplevel-function toplevel-function
                 :identifier-function identifier-function
                 :precheck-function precheck-function
                 :metadata-function metadata-function
                 :validate-function validate-function
                 :prepare-function prepare-function
                 :resume-function resume-function
                 :around-function around-function
                 :fork-guard-function fork-guard-function
                 :probe-runner probe-runner))

(defgeneric checkpoint-create (backend)
  (:documentation "Begin a validated checkpoint and return its pending generation.

The calling process keeps running. The returned generation is :PENDING until a
coordinator process publishes it, at which point a watcher thread in this process
sets it to :READY or :FAILED."))


;;;; -- Fork Mechanism --

(defun checkpoint-single-threaded-p ()
  "Return true when this is the only live Lisp thread at this instant.

This is a snapshot, not synchronization. Callers must prevent thread creation
between this check and fork. A forked child inherits only the forking thread, so
any other live thread would be saved into the core as a thread that no longer
exists."
  (notany (lambda (thread)
            (and (not (eq thread sb-thread:*current-thread*))
                 (sb-thread:thread-alive-p thread)))
          (sb-thread:list-all-threads)))

(defun checkpoint--probe-argument (backend)
  "Return the probe argument BACKEND's runner will pass to a saved core."
  (let ((runner (checkpoint-backend-probe-runner backend)))
    (if (typep runner 'sbcl-core-probe-runner)
        (generation-core-probe-runner-argument runner)
        *default-probe-argument*)))

