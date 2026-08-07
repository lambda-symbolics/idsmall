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
                             (:file "format")))))
