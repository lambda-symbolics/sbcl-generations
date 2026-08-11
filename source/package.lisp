(defpackage #:sbcl-generations
  (:use #:cl)
  (:import-from #:bordeaux-threads
                #:make-thread)
  (:export #:*checkpoint-in-progress-p*
           #:checkpoint-backend
           #:checkpoint-backend-store
           #:checkpoint-create
           #:checkpoint-error
           #:checkpoint-error-cause
           #:checkpoint-error-message
           #:checkpoint-error-pathname
           #:checkpoint-error-stage
           #:checkpoint-resume-toplevel
           #:checkpoint-resume-warning
           #:checkpoint-resume-warning-cause
           #:checkpoint-resume-warning-generation-identifier
           #:checkpoint-single-threaded-p
           #:fork-checkpoint-backend
           #:generation
           #:generation-compatible-p
           #:generation-core-pathname
           #:generation-core-probe-output
           #:generation-core-probe-record
           #:generation-core-probe-run
           #:generation-core-probe-output
           #:generation-core-probe-record
           #:generation-core-probe-runner
           #:generation-core-probe-output
           #:generation-core-probe-record
           #:generation-core-probe-runner-command
           #:generation-coordinator-pid
           #:generation-created-at
           #:generation-directory
           #:generation-find
           #:generation-identifier
           #:generation-list
           #:generation-load-manifest
           #:generation-manifest-pathname
           #:generation-metadata
           #:generation-publish
           #:generation-record-failure
           #:generation-request-rollback
           #:generation-select
           #:generation-selected
           #:generation-status
           #:generation-store
           #:generation-store-current-pathname
           #:generation-store-read-function
           #:generation-store-root
           #:generation-store-write-function
           #:generation-temporary-core-pathname
           #:make-checkpoint-backend
           #:make-generation-store
           #:make-sbcl-core-probe-runner
           #:rollback-requested
           #:rollback-requested-generation-identifier
           #:sbcl-core-probe-runner))

(defpackage #:sbcl-generations/tests
  (:use #:cl)
  (:import-from #:sbcl-generations
                #:*checkpoint-in-progress-p*
                #:checkpoint-create
                #:checkpoint-error
                #:checkpoint-error-stage
                #:checkpoint-single-threaded-p
                #:generation-compatible-p
                #:generation-core-pathname
                #:generation-coordinator-pid
           #:generation-created-at
                #:generation-find
                #:generation-identifier
                #:generation-list
                #:generation-load-manifest
                #:generation-metadata
                #:generation-publish
                #:generation-record-failure
                #:generation-request-rollback
                #:generation-select
                #:generation-selected
                #:generation-status
                #:make-checkpoint-backend
                #:make-generation-store
                #:make-sbcl-core-probe-runner
                #:rollback-requested
                #:rollback-requested-generation-identifier)
  (:export #:run-tests))
