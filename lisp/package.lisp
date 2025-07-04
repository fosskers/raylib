;; NOTE: 2025-07-05 Happy Birthday!
;;
;; Remember, if you're having issues loading this under ECL, load it via the
;; form found in `repl.lisp', not via automated system loading through Sly! The
;; linker flags need to be set properly before this can be loaded, even when
;; doing REPL developement.

(defpackage raylib
  (:use :cl #+sbcl :sb-alien)
  (:local-nicknames (#:tg #:trivial-garbage))
  ;; --- Types --- ;;
  (:export #:vector2 #:make-vector2 #:vector2-x #:vector2-y
           #:rectangle #:make-rectangle #:rectangle-x #:rectangle-y #:rectangle-width #:rectangle-height
           #:color #:make-color #:color-alpha
           #:texture #:texture-width #:texture-height
           #:audio-stream #:sound
           #:music #:music-looping
           #:camera-2d #:make-camera-2d #:camera-2d-offset #:camera-2d-target #:get-world-to-screen-2d
           #:keyboard-key #:gamepad-button)
  ;; --- Functions --- ;;
  (:export #:init-window #:close-window
           #:init-audio-device #:close-audio-device
           #:set-target-fps #:window-should-close
           #:begin-drawing #:end-drawing
           #:begin-mode-2d #:end-mode-2d
           #:clear-background #:draw-fps #:draw-text #:draw-rectangle #:draw-line
           #:load-texture #:unload-texture #:is-texture-valid #:draw-texture #:draw-texture-v #:draw-texture-rec
           #:load-sound #:unload-sound #:play-sound
           #:load-music-stream #:unload-music-stream #:is-music-stream-playing #:play-music-stream #:update-music-stream
           #:is-key-pressed #:is-key-down
           #:is-gamepad-button-pressed #:is-gamepad-button-down #:get-gamepad-name #:is-gamepad-available #:get-gamepad-button-pressed
           #:get-gamepad-axis-count #:get-gamepad-axis-movement
           #:check-collision-recs #:check-collision-point-rec
           #:get-frame-time)
  ;; --- Library Loading --- ;;
  #+sbcl
  (:export #:load-shared-objects)
  (:documentation "A light wrapping of necessary Raylib types and functions."))

(in-package :raylib)

#+sbcl
(defun load-shared-objects (&key (target nil))
  "Dynamically load the necessary `.so' files. This is wrapped as a function so that
downstream callers can call it again as necessary when the Lisp Image is being
restarted. Note the use of `:dont-save' below. This is to allow the package to
be compiled with `.so' files found in one location, but run with ones from another."
  (let ((dir (case target
               (:linux "/usr/lib/")
               (t "lib/"))))
    #+linux
    (progn
      (load-shared-object (merge-pathnames "liblisp-raylib.so" dir) :dont-save t)
      (load-shared-object (merge-pathnames "liblisp-raylib-shim.so" dir) :dont-save t))
    #+win32
    (progn
      (load-shared-object (merge-pathnames "lisp-raylib.dll" dir) :dont-save t)
      (load-shared-object (merge-pathnames "lisp-raylib-shim.dll" dir) :dont-save t))))

#+sbcl
(load-shared-objects)

;; NOTE: 2025-01-03 We preload the shared libraries here to ensure that all
;; functions are already visible when we start to reference them in other files.
#+(and ecl linux)
(progn
  (ffi:load-foreign-library #p"lib/liblisp-raylib.so")
  (ffi:load-foreign-library #p"lib/liblisp-raylib-shim.so"))

;; --- Keyboard and Gamepad --- ;;

;; HACK: 2024-12-29 Hacked manually as I couldn't figure out a first-class way
;; to reference enum values.
(defun keyboard-key (kw)
  (case kw
    ;; --- Directions --- ;;
    (:right 262)
    (:left  263)
    (:down  264)
    (:up    265)
    ;; --- Control --- ;;
    (:space 32)
    (:tab   258)
    (:enter 257)
    (:shift 340)
    ;; --- Numbers --- ;;
    (:zero  48)
    (:one   49)
    ;; --- Letters --- ;;
    (:w     87)
    (:a     65)
    (:s     83)
    (:d     68)
    (:r     82)
    (t (error "Unknown keyboard key: ~a" kw))))

#++
(keyboard-key :kim)

(defun gamepad-button (kw)
  (case kw
    (:unknown 0)
    (:left-face-up 1)
    (:left-face-right 2)
    (:left-face-down 3)
    (:left-face-left 4)
    (:right-face-up 5)
    (:right-face-right 6)
    (:right-face-down 7)
    (:right-face-left 8)
    (:left-trigger-1 9)
    (:left-trigger-2 10)
    (:right-trigger-1 11)
    (:right-trigger-2 12)
    (:middle-left 13)
    (:middle 14)
    (:middle-right 15)
    (:left-thumb 16)
    (:right-thumb 17)
    (t (error "Unknown gamepad button: ~a" kw))))

;; Sanity test: This should work as-is under either compiler.
#+nil
(progn
  (let ((colour (make-color :r 255 :g 255 :b 255 :a 255)))
    (init-window 300 300 "hello!")
    (set-target-fps 100)
    (loop while (not (window-should-close))
          do (progn (begin-drawing)
                    (clear-background colour)
                    (draw-fps 0 0)
                    (end-drawing)))
    (close-window)))
