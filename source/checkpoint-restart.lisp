(in-package #:sbcl-generations)

;;;; -- Supervised Restart Checkpoints --

(defclass restart-checkpoint-backend (checkpoint-backend)
  ((pending-generation
    :initform nil
    :accessor checkpoint-restart--pending-generation
    :documentation "The reserved generation, or NIL before a request."))
  (:documentation "An exact heap checkpoint saved by a supervised process."))

(defun make-restart-checkpoint-backend
    (&key store toplevel-function
       (identifier-function #'checkpoint--default-identifier)
       precheck-function metadata-function validate-function prepare-function
       resume-function around-function fork-guard-function probe-runner)
  "Create a backend whose host supervises an explicit save and restart.

The hook arguments match MAKE-CHECKPOINT-BACKEND. CREATE reserves an identity
without quiescing the application. SAVE-AND-EXIT reruns prechecks, then validates
and refreshes metadata inside the guard at the actual save boundary. The host
must deliver its response before invoking that operation. A separate supervisor
must probe and publish the temporary core with GENERATION-PUBLISH before booting
it. No reconstruction or fork is used."
  (unless (typep store 'generation-store)
    (generations--fail :backend "A checkpoint backend needs a generation store."))
  (unless (functionp toplevel-function)
    (generations--fail :backend "A checkpoint backend needs a toplevel function."))
  (make-instance 'restart-checkpoint-backend
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

(defun checkpoint-restart--call (hook thunk)
  "Call THUNK within optional host HOOK."
  (if hook (funcall hook thunk) (funcall thunk)))

(defun checkpoint-restart-create (backend)
  "Reserve and return a pending generation without stopping the application.

Only precheck and metadata hooks run. The application may durably deliver this
identity to its user before saving. A second request on this backend is refused."
  (unless (typep backend 'restart-checkpoint-backend)
    (generations--fail :backend "Expected a restart checkpoint backend."))
  (let* ((precheck (checkpoint-backend-precheck-function backend))
         (value (and precheck (funcall precheck)))
         (metadata (checkpoint-backend-metadata-function backend)))
    (checkpoint-restart--call
     (checkpoint-backend-around-function backend)
     (lambda ()
       (checkpoint-restart--call
        (checkpoint-backend-fork-guard-function backend)
        (lambda ()
          (when (or *checkpoint-in-progress-p*
                    (checkpoint-restart--pending-generation backend))
            (generations--fail :validation "A checkpoint is already pending."))
          (let ((identifier
                  (funcall (checkpoint-backend-identifier-function backend))))
            (setf (checkpoint-restart--pending-generation backend)
                  (generation--create-record
                   (checkpoint-backend-store backend) identifier
                   :metadata (and metadata (funcall metadata identifier value)))))))))))

(defmethod checkpoint-create ((backend restart-checkpoint-backend))
  "Reserve a restart checkpoint; the host schedules the actual safe boundary."
  (checkpoint-restart-create backend))

(defun checkpoint-restart-save-and-exit (backend generation &key ready-function)
  "Save the exact heap as GENERATION and terminate this process.

READY-FUNCTION receives GENERATION after final validation and metadata refresh,
before resource detachment. Use it to atomically write the supervisor's durable
restart envelope. The supervisor must wait for process exit, probe, publish,
and restart the core. Successful save exits zero. Any failure records a failure
artifact, attempts the resume hook when validation completed, and exits one."
  (unless (and (typep backend 'restart-checkpoint-backend)
               (eq generation (checkpoint-restart--pending-generation backend)))
    (generations--fail :backend "Generation was not reserved by this backend."))
  (let ((state nil)
        (validated-p nil))
    (handler-case
        (let* ((precheck (checkpoint-backend-precheck-function backend))
               (value (and precheck (funcall precheck))))
          (checkpoint-restart--call
           (checkpoint-backend-around-function backend)
           (lambda ()
             (checkpoint-restart--call
              (checkpoint-backend-fork-guard-function backend)
              (lambda ()
                (when *checkpoint-in-progress-p*
                  (generations--fail :validation "A checkpoint is already being published."))
                (let ((validate (checkpoint-backend-validate-function backend))
                      (metadata (checkpoint-backend-metadata-function backend)))
                  (when validate
                    (setf state (funcall validate value)))
                  (setf validated-p t)
                  (when metadata
                    (setf (slot-value generation 'metadata)
                          (copy-list (funcall metadata
                                              (generation-identifier generation)
                                              value)))))
                (ensure-directories-exist
                 (generation-temporary-core-pathname generation))
                (when ready-function
                  (funcall ready-function generation))
                (setf (checkpoint-restart--pending-generation backend) nil
                      *checkpoint-in-progress-p* nil
                      *checkpoint-core-probe-record* (generation-core-probe-record generation)
                      *checkpoint-core-probe-argument* (checkpoint--probe-argument backend)
                      *checkpoint-core-toplevel-function*
                      (checkpoint-backend-toplevel-function backend))
                (let ((prepare (checkpoint-backend-prepare-function backend)))
                  (when prepare (funcall prepare generation)))
                (unless (checkpoint-single-threaded-p)
                  (generations--fail :save "Other Lisp threads survived checkpoint preparation."))
                (finish-output *standard-output*)
                (finish-output *error-output*)
                (sb-ext:gc :full t)
                (sb-ext:save-lisp-and-die
                 (namestring (generation-temporary-core-pathname generation))
                 :toplevel #'checkpoint-resume-toplevel
                 :executable nil :purify nil :compression nil))))))
      (serious-condition (condition)
        (setf (generation-status generation) :failed)
        (ignore-errors
          (generation-record-failure (checkpoint-backend-store backend)
                                     generation :save condition))
        (when validated-p
          (let ((resume (checkpoint-backend-resume-function backend)))
            (when resume (ignore-errors (funcall resume state)))))
        (sb-ext:exit :code 1))))
  nil)
