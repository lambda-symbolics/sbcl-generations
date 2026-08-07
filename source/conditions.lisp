(in-package #:sbcl-generations)

;;;; -- Conditions --

(define-condition checkpoint-error (error)
  ((message
    :initarg :message
    :reader checkpoint-error-message
    :type string
    :documentation "The human-readable description of the failure.")
   (stage
    :initarg :stage
    :reader checkpoint-error-stage
    :type keyword
    :documentation "The checkpoint stage that refused to continue.")
   (pathname
    :initarg :pathname
    :initform nil
    :reader checkpoint-error-pathname
    :type (or null pathname)
    :documentation "The artifact involved in the failure, when one is known.")
   (cause
    :initarg :cause
    :initform nil
    :reader checkpoint-error-cause
    :type t
    :documentation "The underlying condition, when the failure wrapped one."))
  (:report
   (lambda (condition stream)
     (format stream "~A (~S)"
             (checkpoint-error-message condition)
             (checkpoint-error-stage condition))))
  (:documentation "A checkpoint or generation operation refused to continue.

STAGE is stable and suitable for a host to translate. The defined stages are
:BACKEND, :VALIDATION, :FORK, :SAVE, :SAVER-EXIT, :COORDINATOR, :PROBE,
:PUBLISH, :MANIFEST, and :SELECTION."))

(define-condition rollback-requested (error)
  ((message
    :initarg :message
    :reader rollback-requested-message
    :type string
    :documentation "The human-readable description of the request.")
   (generation-identifier
    :initarg :generation-identifier
    :reader rollback-requested-generation-identifier
    :type string
    :documentation "The generation the process should restart into."))
  (:report
   (lambda (condition stream)
     (write-string (rollback-requested-message condition) stream)))
  (:documentation "A selected generation should replace this running process.

This is signaled rather than performed, because only the process's own launcher
knows how to restart it. A host establishes a handler at the point where exiting
is safe."))

(define-condition checkpoint-resume-warning (warning)
  ((generation-identifier
    :initarg :generation-identifier
    :reader checkpoint-resume-warning-generation-identifier
    :type string
    :documentation "The generation whose checkpoint had already been forked.")
   (cause
    :initarg :cause
    :reader checkpoint-resume-warning-cause
    :type t
    :documentation "The condition that interrupted the host's resume hook."))
  (:report
   (lambda (condition stream)
     (format stream "Checkpoint ~A forked, but resuming this process failed: ~A"
             (checkpoint-resume-warning-generation-identifier condition)
             (checkpoint-resume-warning-cause condition))))
  (:documentation "A checkpoint was forked, but the host could not fully resume.

This is a warning rather than an error because the coordinator is already
publishing the generation. Failing here would report a checkpoint that did not
happen."))

(defun generations--fail (stage message &key pathname cause)
  "Signal a CHECKPOINT-ERROR for STAGE reporting MESSAGE."
  (error 'checkpoint-error
         :message message
         :stage stage
         :pathname pathname
         :cause cause))
