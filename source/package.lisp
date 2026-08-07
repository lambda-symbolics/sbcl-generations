(defpackage #:sbcl-generations
  (:use #:cl)
  (:import-from #:bordeaux-threads
                #:make-thread)
  (:import-from #:sexp-store
                #:snapshot-read
                #:snapshot-write)
  (:export #:*checkpoint-in-progress-p*
           #:checkpoint-backend
           #:checkpoint-backend-store
           #:checkpoint-create
           #:checkpoint-error
           #:checkpoint-error-cause
           #:checkpoint-error-pathname
           #:checkpoint-error-stage
           #:checkpoint-resume-toplevel
           #:checkpoint-resume-warning
           #:checkpoint-resume-warning-cause
           #:checkpoint-resume-warning-generation-identifier
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
           #:generation-store-root
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
                #:generation-compatible-p
                #:generation-core-pathname
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
