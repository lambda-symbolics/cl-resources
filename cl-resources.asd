(asdf:defsystem "cl-resources"
  :description "Authority-neutral revisioned resources and recoverable change sets"
  :author "Lambda Symbolics"
  :license "ISC"
  :version "0.1.0"
  :depends-on ("bordeaux-threads")
  :serial t
  :components ((:file "src/package")
               (:file "src/protocol")
               (:file "src/change-set")
               (:file "src/callback-adapter"))
  :in-order-to ((asdf:test-op (asdf:test-op "cl-resources/tests"))))

(asdf:defsystem "cl-resources/tests"
  :depends-on ("cl-resources")
  :serial t
  :components ((:file "tests/tests")
               (:file "tests/revision-guard")
               (:file "tests/concurrency")
               (:file "tests/stage-ownership"))
  :perform (asdf:test-op (operation system)
             (declare (ignore operation system))
             (uiop:symbol-call :cl-resources :run-tests)))
