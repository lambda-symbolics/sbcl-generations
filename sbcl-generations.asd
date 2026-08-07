(asdf:defsystem #:sbcl-generations
  :description "Non-stopping fork checkpoints and retained SBCL image generations"
  :author "Lukáš Hozda"
  :license "COLL-Attribution"
  :version "0.1.0"
  :serial t
  :depends-on (#:bordeaux-threads
               #:sb-posix)
  :components ((:module "source"
                :serial t
                :components ((:file "package")
                             (:file "conditions")
                             (:file "generation")
                             (:file "store")
                             (:file "publish")
                             (:file "checkpoint"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:sbcl-generations/tests))))

(asdf:defsystem #:sbcl-generations/tests
  :description "Tests for sbcl-generations"
  :depends-on (#:sbcl-generations)
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "tests"))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:sbcl-generations/tests '#:run-tests)))
