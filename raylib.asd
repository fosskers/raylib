(defsystem "raylib"
  :depends-on (:trivial-garbage)
  :serial t
  :components ((:module "lisp"
                :components ((:file "package")
                             (:file "sbcl"))))
  :description "Custom bindings to Raylib C.")
