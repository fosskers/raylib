#+ecl
(progn
  (let* ((path (merge-pathnames "lib/" (ext:getcwd)))
         (args (format nil "-Wl,-rpath,~a -L~a" path path)))
    (setf c:*user-linker-flags* args)
    (setf c:*user-linker-libs*  "-llisp-raylib -llisp-raylib-shim"))
  (asdf:load-system :raylib :force t))
