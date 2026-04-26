(defsystem "raylib"
  :version "0.0.0"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "MPL-2.0"
  :depends-on ()
  :serial t
  :components ((:module "lisp"
                :components ((:file "package")
                             (:file "sbcl" :if-feature :sbcl))))
  :description "Custom bindings to Raylib C.")

