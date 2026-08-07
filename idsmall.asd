(asdf:defsystem #:idsmall
  :description "Short Base58 identifiers holding a reversibly scrambled timestamp"
  :author "Lukáš Hozda"
  :license "COLL-Attribution"
  :version "0.1.0"
  :serial t
  :depends-on (#:bordeaux-threads)
  :components ((:module "source"
                :serial t
                :components ((:file "package")
                             (:file "conditions")
                             (:file "format")
                             (:file "allocation"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:idsmall/tests))))

(asdf:defsystem #:idsmall/tests
  :description "Tests for idsmall"
  :depends-on (#:idsmall)
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "tests"))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:idsmall/tests '#:run-tests)))
