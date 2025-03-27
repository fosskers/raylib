PLATFORM ?= PLATFORM_DESKTOP_GLFW

dev: lib/ lib/liblisp-raylib.so lib/liblisp-raylib-shim.so raylib.h shim.h

lib/:
	mkdir lib/

ecl: dev lib/liblisp-raylib.so.550

lib/liblisp-raylib.so.550:
	ln -s liblisp-raylib.so lib/liblisp-raylib.so.550

lib/liblisp-raylib.so:
	cd vendored/raylib-c/src/ && $(MAKE) PLATFORM=$(PLATFORM)
	mv vendored/raylib-c/src/liblisp-raylib.so lib/liblisp-raylib.so

lib/liblisp-raylib-shim.so: c/shim.c c/raylib.h
	cd c && gcc -O3 -fPIC -shared -o liblisp-raylib-shim.so shim.c
	mv c/liblisp-raylib-shim.so lib/

c/raylib.h:
	ln -s ../vendored/raylib-c/src/raylib.h c/raylib.h

raylib.h:
	ln -s vendored/raylib-c/src/raylib.h raylib.h

shim.h:
	ln -s c/shim.h shim.h

clean:
	-rm raylib.h shim.h c/raylib.h
	rm -rf lib/
	cd vendored/raylib-c/src/ && $(MAKE) clean
