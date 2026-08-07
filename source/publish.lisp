(in-package #:sbcl-generations)

;;;; -- Core Probing --

(defparameter *default-probe-argument*
  "--sbcl-generations-internal-core-probe"
  "The private argument a saved core answers with its embedded identity.

A host may replace it, but the value must stay one argument that no ordinary
invocation would pass, because a core that sees it prints its identity and
exits instead of running.")

(defclass generation-core-probe-runner ()
  ()
  (:documentation "The boundary used to ask an unpublished core who it is."))

(defclass sbcl-core-probe-runner (generation-core-probe-runner)
  ((command
    :initarg :command
    :reader generation-core-probe-runner-command
    :type string
    :documentation "The SBCL executable used to boot an unpublished core.")
   (probe-argument
    :initarg :probe-argument
    :reader generation-core-probe-runner-argument
    :type string
    :documentation "The private argument passed to the booted core."))
  (:documentation "A probe runner that boots the core in a separate process."))

(defun make-sbcl-core-probe-runner
    (&key command (probe-argument *default-probe-argument*))
  "Create a probe runner that boots an unpublished core with SBCL.

COMMAND defaults to the SBCL_GENERATIONS_SBCL environment variable when it is
set, and to \"sbcl\" otherwise. A host that ships its own runtime should pass
the runtime it will actually boot the core with, since a core is only usable by
the build that saved it."
  (let ((configured (uiop:getenv "SBCL_GENERATIONS_SBCL")))
    (make-instance 'sbcl-core-probe-runner
                   :command (or command
                                (and configured
                                     (plusp (length configured))
                                     configured)
                                "sbcl")
                   :probe-argument probe-argument)))

(defgeneric generation-core-probe-run (runner generation)
  (:documentation
   "Boot GENERATION's unpublished core through RUNNER and return its output."))

(defmethod generation-core-probe-run
    ((runner sbcl-core-probe-runner) (generation generation))
  "Boot GENERATION's unpublished core and capture its internal probe output."
  (handler-case
      (uiop:run-program
       (list (generation-core-probe-runner-command runner)
             "--noinform"
             "--core"
             (namestring (generation-temporary-core-pathname generation))
             "--end-runtime-options"
             (generation-core-probe-runner-argument runner))
       :input nil
       :output :string
       :error-output :output)
    (error ()
      (generations--fail :probe
                         "The saved core could not run its internal probe."
                         :pathname
                         (generation-temporary-core-pathname generation)))))

(defun generation--validate-core-probe (generation runner)
  "Require RUNNER to report GENERATION's exact embedded identity.

This is what makes publication safe. A core is a whole heap written by a forked
child, and the only evidence that the child saved the image this generation
describes is the core saying so when booted."
  (let ((actual (handler-case
                    (generation-core-probe-run runner generation)
                  (checkpoint-error (condition)
                    (error condition))
                  (error ()
                    (generations--fail
                     :probe
                     "The core probe failed unexpectedly."
                     :pathname
                     (generation-temporary-core-pathname generation))))))
    (unless (and (stringp actual)
                 (string= actual
                          (generation-core-probe-output
                           (generation-core-probe-record generation))))
      (generations--fail :probe
                         "The saved core reported the wrong generation identity."
                         :pathname
                         (generation-temporary-core-pathname generation))))
  nil)


;;;; -- Publication --

(defun generation-publish (store generation &key probe-runner)
  "Validate and publish GENERATION's core, manifest, and selection pointer.

The order matters. The core is probed while it is still unpublished, then
renamed over its final name, then described by its manifest, and only then named
by the selection pointer. An interruption at any point leaves the previous
selection intact."
  (let ((runner (or probe-runner (make-sbcl-core-probe-runner))))
    (unless (probe-file (generation-temporary-core-pathname generation))
      (generations--fail :publish
                         "The checkpoint saver produced no core file."
                         :pathname
                         (generation-temporary-core-pathname generation)))
    (let ((validator (generation-store-publish-validator store)))
      (when validator
        (funcall validator generation)))
    (generation--validate-core-probe generation runner)
    (uiop:rename-file-overwriting-target
     (generation-temporary-core-pathname generation)
     (generation-core-pathname generation))
    (generation--write-form store
                            (generation-manifest-pathname generation)
                            (generation-manifest-form generation store))
    (generation--write-form store
                            (generation-store-current-pathname store)
                            (generation--select-form generation))
    (setf (generation-status generation) :ready)
    generation))
