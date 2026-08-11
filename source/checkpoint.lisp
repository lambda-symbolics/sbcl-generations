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

(defun checkpoint--save-core (backend generation)
  "Write GENERATION's core from inside the saver child, then exit.

This function never returns. SAVE-LISP-AND-DIE ends the child, and any failure
before that is recorded beside the generation so the parent can explain it."
  (handler-case
      (progn
        (setf *checkpoint-in-progress-p* nil
              *checkpoint-core-probe-record*
              (generation-core-probe-record generation)
              *checkpoint-core-probe-argument*
              (checkpoint--probe-argument backend)
              *checkpoint-core-toplevel-function*
              (checkpoint-backend-toplevel-function backend))
        (let ((prepare (checkpoint-backend-prepare-function backend)))
          (when prepare
            (funcall prepare generation)))
        (sb-ext:gc :full t)
        (sb-ext:save-lisp-and-die
         (namestring (generation-temporary-core-pathname generation))
         :toplevel #'checkpoint-resume-toplevel
         :executable nil
         :purify nil
         :compression nil))
    (error (condition)
      (ignore-errors (generation-record-failure
                      (checkpoint-backend-store backend) generation
                      :save condition))
      (sb-posix:_exit 1)))
  nil)

(defun checkpoint--coordinate (backend generation)
  "Fork the saver, publish its artifacts, and exit this coordinator.

The coordinator exists so the parent never waits. It is the process that blocks
on the saver and then publishes, and it exits without returning to any host code."
  (handler-case
      (let ((saver-pid (sb-posix:fork)))
        (if (zerop saver-pid)
            (checkpoint--save-core backend generation)
            (multiple-value-bind (waited-pid status)
                (sb-posix:waitpid saver-pid 0)
              (if (and (= waited-pid saver-pid)
                       (sb-posix:wifexited status)
                       (zerop (sb-posix:wexitstatus status)))
                  (progn
                    (generation-publish (checkpoint-backend-store backend)
                                        generation
                                        :probe-runner
                                        (checkpoint-backend-probe-runner backend))
                    (sb-posix:_exit 0))
                  (progn
                    (unless (probe-file
                             (merge-pathnames "failure.sexp"
                                              (generation-directory generation)))
                      (generation-record-failure
                       (checkpoint-backend-store backend)
                       generation
                       :saver-exit
                       (if (sb-posix:wifexited status)
                           (format nil "Saver exited with status ~D."
                                   (sb-posix:wexitstatus status))
                           (format nil "Saver terminated with status word ~D."
                                   status))))
                    (sb-posix:_exit 1))))))
    (error (condition)
      (ignore-errors (generation-record-failure
                      (checkpoint-backend-store backend) generation
                      :coordinator condition))
      (sb-posix:_exit 1)))
  nil)

(defun checkpoint--watch-coordinator (generation coordinator-pid)
  "Record GENERATION's outcome in this process once COORDINATOR-PID exits."
  (unwind-protect
       (handler-case
           (multiple-value-bind (waited-pid status)
               (sb-posix:waitpid coordinator-pid 0)
             (setf (generation-status generation)
                   (if (and (= waited-pid coordinator-pid)
                            (sb-posix:wifexited status)
                            (zerop (sb-posix:wexitstatus status)))
                       :ready
                       :failed)))
         (error ()
           (setf (generation-status generation) :failed)))
    (setf *checkpoint-in-progress-p* nil))
  nil)

(defun checkpoint--fork (generation)
  "Fork the coordinator for GENERATION and report which process this is.

Return two values: true in the coordinator child, and the coordinator identifier
in the parent."
  (let ((coordinator-p nil)
        (coordinator-pid nil))
    (ensure-directories-exist (generation-directory generation))
    (setf *checkpoint-in-progress-p* t)
    (handler-case
        (let ((pid (sb-posix:fork)))
          (if (zerop pid)
              (setf coordinator-p t)
              (setf coordinator-pid pid
                    (generation-coordinator-pid generation) pid)))
      (error (condition)
        (setf *checkpoint-in-progress-p* nil)
        (generations--fail :fork
                           (format nil "Could not fork a checkpoint coordinator: ~A"
                                   condition)
                           :pathname (generation-directory generation))))
    (values coordinator-p coordinator-pid)))

(defmethod checkpoint-create ((backend fork-checkpoint-backend))
  "Fork a coordinator while briefly exclusive, then let the parent continue."
  (let ((around (checkpoint-backend-around-function backend)))
    (if around
        (funcall around (lambda () (checkpoint--create backend)))
        (checkpoint--create backend))))

(defun checkpoint--create (backend)
  "Create one checkpoint, assuming any host dynamic context is established."
  (let* ((store (checkpoint-backend-store backend))
         (precheck (checkpoint-backend-precheck-function backend))
         (metadata-function (checkpoint-backend-metadata-function backend))
         (validate (checkpoint-backend-validate-function backend))
         (guard (checkpoint-backend-fork-guard-function backend))
         (precheck-value (and precheck (funcall precheck)))
         (generation nil)
         (validated-state nil)
         (coordinator-p nil)
         (coordinator-pid nil)
         (failure nil))
    (flet ((region ()
             ;; Everything here runs while the host holds the process exclusive,
             ;; so it stays in the order a checkpoint actually depends on: refuse
             ;; a second checkpoint, let the host re-validate, confirm this is the
             ;; only thread, then record the generation and fork.
             (when *checkpoint-in-progress-p*
               (generations--fail :validation
                                  "A checkpoint is already being published."))
             (when validate
               (setf validated-state (funcall validate precheck-value)))
             (unless (checkpoint-single-threaded-p)
               (generations--fail
                :fork
                "A checkpoint requires the current thread to be the only live thread."))
             (finish-output *standard-output*)
             (finish-output *error-output*)
             (let ((identifier
                     (funcall (checkpoint-backend-identifier-function backend))))
               (setf generation
                     (generation--create-record
                      store identifier
                      :metadata (and metadata-function
                                     (funcall metadata-function
                                              identifier precheck-value)))))
             (multiple-value-setq (coordinator-p coordinator-pid)
               (checkpoint--fork generation))
             nil))
      (handler-case
          (if guard
              (funcall guard #'region)
              (region))
        (serious-condition (condition)
          (setf failure
                (if (typep condition 'checkpoint-error)
                    condition
                    (make-condition 'checkpoint-error
                                    :message "Checkpoint preparation failed."
                                    :stage :fork
                                    :cause condition))))))
    ;; The saver and coordinator must not resume host resources: they are about
    ;; to write or publish an image and then exit.
    (let ((resume (checkpoint-backend-resume-function backend)))
      (when (and resume (not coordinator-p))
        (handler-case
            (funcall resume validated-state)
          (serious-condition (condition)
            (unless failure
              (setf failure
                    (make-condition 'checkpoint-error
                                    :message "Could not resume after checkpointing."
                                    :stage :fork
                                    :cause condition)))))))
    (cond
      (coordinator-p
       (checkpoint--coordinate backend generation))
      (coordinator-pid
       (make-thread (lambda ()
                      (checkpoint--watch-coordinator generation coordinator-pid))
                    :name (format nil "sbcl-generations checkpoint ~A"
                                  (generation-identifier generation)))
       (when failure
         (warn 'checkpoint-resume-warning
               :generation-identifier (generation-identifier generation)
               :cause failure))
       generation)
      (t
       (error (or failure
                  (make-condition 'checkpoint-error
                                  :message
                                  "Checkpoint preparation ended without a coordinator."
                                  :stage :fork)))))))
