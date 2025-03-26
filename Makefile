PLATFORM ?= PLATFORM_DESKTOP_GLFW

dev: lib/ lib/liblisp-raylib.so lib/liblisp-shim.so raylib.h

lib/:
	mkdir lib/

lib/liblisp-raylib.so.550:
	ln -s liblisp-raylib.so lib/liblisp-raylib.so.550

lib/liblisp-raylib.so:
	cd vendored/raylib-c/src/ && $(MAKE) PLATFORM=$(PLATFORM)
	mv vendored/raylib-c/src/liblisp-raylib.so lib/liblisp-raylib.so

lib/liblisp-shim.so: c/shim.c c/raylib.h
	cd c && gcc -O3 -fPIC -shared -o liblisp-shim.so shim.c
	mv c/liblisp-shim.so lib/

c/raylib.h:
	ln -s ../vendored/raylib-c/src/raylib.h c/raylib.h

raylib.h:
	ln -s vendored/raylib-c/src/raylib.h raylib.h

clean:
	-rm raylib.h lisp c/raylib.h
	rm -rf lib/
	cd vendored/raylib-c/src/ && $(MAKE) clean
