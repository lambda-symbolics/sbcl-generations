(in-package #:sbcl-generations)

;;;; -- Fork Mechanism --

;;; The coordinator and saver forks that make a checkpoint non-stopping. This
;;; file is only loaded where SBCL provides fork; MAKE-CHECKPOINT-BACKEND
;;; refuses other hosts before any of it is needed.

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
