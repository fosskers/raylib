(defsystem "raylib"
  :depends-on (:trivial-garbage)
  :serial t
  :components ((:module "lisp"
                :components ((:file "package")
                             (:file "sbcl" :if-feature :sbcl)
                             (:file "ecl"  :if-feature :ecl))))
  :description "Custom bindings to Raylib C.")

