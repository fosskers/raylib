(in-package :raylib)

;; For access to my various `_Foo' functions.
(ffi:clines "#include \"shim.h\"")
;; For access to `free'.
(ffi:clines "#include <stdlib.h>")

;; NOTE: 2025-01-03 This is highly bespoke and comes directly from the maintainer of ECL.
(defun free! (ptr)
  "A custom call to C's `free' that ensures everything is properly reset."
  (ffi:c-inline (ptr) (:object) :void
                "void *ptr = ecl_foreign_data_pointer_safe(#0);
                 #0->foreign.size = 0;
                 #0->foreign.data = NULL;
                 free(ptr);" :one-liner nil))

;; --- Vectors --- ;;

(ffi:def-struct vector2-raw
    (x :float)
  (y :float))

(ffi:def-function ("_MakeVector2" make-vector2-raw)
    ((x :float)
     (y :float))
  :returning (* vector2-raw))

(defstruct (vector2 (:constructor @vector2))
  (pointer nil :type si:foreign-data))

(defun make-vector2 (&key x y)
  (let* ((ptr (make-vector2-raw x y))
         (v   (@vector2 :pointer ptr)))
    (tg:finalize v (lambda () (free! ptr)))))

#++
(make-vector2 :x 1.0 :y 2.0)

(defmacro vector2-x (v)
  "The X slot of a `Vector2'."
  `(ffi:get-slot-value (vector2-pointer ,v) 'vector2-raw 'x))

(defmacro vector2-y (v)
  "The Y slot of a `Vector2'."
  `(ffi:get-slot-value (vector2-pointer ,v) 'vector2-raw 'y))

#++
(let ((v (make-vector2 :x 1.0 :y 2.0)))
  (setf (vector2-x v) 1000.0)
  (vector2-x v))

;; --- Rectangles --- ;;

(ffi:def-struct rectangle-raw
    (x :float)
  (y :float)
  (width :float)
  (height :float))

(ffi:def-function ("_MakeRectangle" make-rectangle-raw)
    ((x :float)
     (y :float)
     (width :float)
     (height :float))
  :returning (* rectangle-raw))

(defstruct (rectangle (:constructor @rectangle))
  (pointer nil :type si:foreign-data))

(defun make-rectangle (&key x y width height)
  (let* ((pointer (make-rectangle-raw x y width height))
         (rect    (@rectangle :pointer pointer)))
    (tg:finalize rect (lambda () (free! pointer)))))

(defmacro rectangle-x (r)
  `(ffi:get-slot-value (rectangle-pointer ,r) 'rectangle-raw 'x))

(defmacro rectangle-y (r)
  `(ffi:get-slot-value (rectangle-pointer ,r) 'rectangle-raw  'y))

(defmacro rectangle-width (r)
  `(ffi:get-slot-value (rectangle-pointer ,r) 'rectangle-raw 'width))

(defmacro rectangle-height (r)
  `(ffi:get-slot-value (rectangle-pointer ,r) 'rectangle-raw 'height))

;; --- Colour --- ;;

(ffi:def-struct color-raw
    (r :unsigned-byte)
  (g :unsigned-byte)
  (b :unsigned-byte)
  (a :unsigned-byte))

(ffi:def-function ("_MakeColor" make-color-raw)
    ((r :unsigned-byte)
     (g :unsigned-byte)
     (b :unsigned-byte)
     (a :unsigned-byte))
  :returning (* color-raw))

(defstruct (color (:constructor @color))
  (pointer nil :type si:foreign-data))

(defun make-color (&key r g b a)
  (let* ((pointer (make-color-raw r g b a))
         (color   (@color :pointer pointer)))
    (tg:finalize color (lambda () (free! pointer)))))

(ffi:def-function ("_ColorAlpha" color-alpha-raw)
    ((color (* color-raw))
     (alpha :float))
  :returning (* color-raw))

(defun color-alpha (color alpha)
  (let* ((ptr (color-alpha-raw (color-pointer color) alpha))
         (new (@color :pointer ptr)))
    (tg:finalize new (lambda () (free! ptr)))))

;; --- Textures --- ;;

(ffi:def-struct texture-raw
    (id :unsigned-int)
  (width   :int)
  (height  :int)
  (mipmaps :int)
  (format  :int))

(defstruct (texture (:constructor @texture))
  (pointer nil :type si:foreign-data))

(ffi:def-function ("_LoadTexture" load-texture-raw)
    ((file-name :cstring))
  :returning (* texture-raw))

(defun load-texture (file-name)
  (let* ((pointer (load-texture-raw (namestring file-name)))
         (texture (@texture :pointer pointer)))
    ;; TODO: 2025-01-03 Free the c-string here too if necessary.
    (tg:finalize texture (lambda () (free! pointer)))))

(ffi:def-function ("_UnloadTexture" unload-texture-raw)
    ((texture (* texture-raw)))
  :returning :void)

(defun unload-texture (texture)
  (unload-texture-raw (texture-pointer texture)))

#++
(progn
  (init-window 300 300 "hello!")
  (let ((p (load-texture-raw "assets/logo.png")))
    (format t "~a~%" (slot p 'id))
    (format t "~a~%" (slot p 'width))
    (format t "~a~%" (slot p 'height)))
  (close-window))

(defmacro texture-width (r)
  `(ffi:get-slot-value (texture-pointer ,r) 'texture-raw 'width))

(defmacro texture-height (r)
  `(ffi:get-slot-value (texture-pointer ,r) 'texture-raw 'height))

(ffi:def-function ("_IsTextureValid" is-texture-valid-raw)
    ((texture (* texture-raw)))
  ;; HACK: 2025-01-03 ECL has no `:bool' type specifier? Luckily I happen to
  ;; know that the `bool' from `stdbool' is 8-bit.
  :returning :unsigned-byte)

(defun is-texture-valid (texture)
  (= 1 (is-texture-valid-raw (texture-pointer texture))))

(ffi:def-function ("_DrawTexture" draw-texture-raw)
    ((texture (* texture-raw))
     (pos-x :int)
     (pos-y :int)
     (tint (* color-raw)))
  :returning :void)

(defun draw-texture (texture pos-x pos-y tint)
  (draw-texture-raw (texture-pointer texture)
                    pos-x pos-y
                    (color-pointer tint)))

(ffi:def-function ("_DrawTextureV" draw-texture-v-raw)
    ((texture  (* texture-raw))
     (position (* vector2-raw))
     (tint     (* color-raw)))
  :returning :void)

(defun draw-texture-v (texture position tint)
  (draw-texture-v-raw (texture-pointer texture)
                      (vector2-pointer position)
                      (color-pointer tint)))

(ffi:def-function ("_DrawTextureRec" draw-texture-rec-raw)
    ((texture  (* texture-raw))
     (source   (* rectangle-raw))
     (position (* vector2-raw))
     (tint     (* color-raw)))
  :returning :void)

(defun draw-texture-rec (texture source position tint)
  (draw-texture-rec-raw (texture-pointer texture)
                        (rectangle-pointer source)
                        (vector2-pointer position)
                        (color-pointer tint)))

;; --- Fonts --- ;;

;; TODO: 2025-08-07 Start here. Port over the `font' type and all its
;; associated functions. Ensure the main example still works. All the stuff in
;; `shim' has already been taken care of.

(ffi:def-struct font-raw
    (base-size   :int)
  (glyph-count   :int)
  (glyph-padding :int)
  (texture       texture-raw)
  (recs          :pointer-void)
  (glyphs        :pointer-void))

(defstruct (font (:constructor @font))
  (pointer nil :type si:foreign-data))

(ffi:def-function ("_LoadFont" load-font-raw)
    ((file-name :cstring))
  :returning (* font-raw))

(declaim (ftype (function ((or string pathname)) font) load-font))
(defun load-font (file-name)
  "Load a font from a file into GPU memory."
  (let* ((pointer (load-font-raw (namestring file-name)))
         (font (@font :pointer pointer)))
    (tg:finalize font (lambda () (free! pointer)))))

(ffi:def-function ("_LoadFontEx" load-font-ex-raw)
    ((file-name  :cstring)
     (font-size  :int)
     (codepoints (* :int))
     (codepoint-count :int))
  :returning (* font-raw))

(declaim (ftype (function ((or string pathname) fixnum (vector integer)) font) load-font-ex))
(defun load-font-ex (file-name font-size codepoints)
  "Load a font with additional parameters, particularly those pertaining to UTF-8
codepoints. The `codepoints' vector should contain the result of `char-code' for
every character you intend to print in your game."
  (let* ((count (length codepoints))
         (array (ffi:allocate-foreign-object :int count)))
    ;; FIXME: 2025-08-07 There may be a way to avoid this manual copy.
    ;; (loop :for i :from 0 :below count
    ;;       :do (setf (ffi:deref-array array :int i) (aref codepoints i)))
    (let* ((point (load-font-ex-raw (namestring file-name) font-size array count))
           (font  (@font :pointer point)))
      (ffi:free-foreign-object array)
      (tg:finalize font (lambda () (free! point))))))

(ffi:def-function ("_IsFontValid" is-font-valid-raw)
    ((font (* font-raw)))
  ;; HACK: 2025-08-09 Bool.
  :returning :unsigned-byte)

(ffi:def-function ("_UnloadFont" unload-font-raw)
    ((font (* font-raw)))
  :returning :void)

(declaim (ftype (function (font)) unload-font))
(defun unload-font (font)
  "Unload a font from GPU memory."
  (unload-font-raw (font-pointer font)))

(declaim (ftype (function (font) boolean) is-font-valid))
(defun is-font-valid (font)
  "Did a given `font' load correctly?"
  (= 1 (is-font-valid-raw (font-pointer font))))

;; --- Sounds and Music --- ;;

(ffi:def-struct audio-stream
    (buffer    :pointer-void)
  (processor   :pointer-void)
  (sample-rate :unsigned-int)
  (sample-size :unsigned-int)
  (channels    :unsigned-int))

(ffi:def-struct sound-raw
    (stream audio-stream)
  (frame-count :unsigned-int))

(defstruct (sound (:constructor @sound))
  (pointer nil :type si:foreign-data))

(ffi:def-function ("_LoadSound" load-sound-raw)
    ((file-name :cstring))
  :returning (* sound-raw))

(defun load-sound (file-name)
  (let* ((pointer (load-sound-raw (namestring file-name)))
         (sound   (@sound :pointer pointer)))
    (tg:finalize sound (lambda () (free! pointer)))))

(ffi:def-function ("_UnloadSound" unload-sound-raw)
    ((sound (* sound-raw)))
  :returning :void)

(defun unload-sound (sound)
  (unload-sound-raw (sound-pointer sound)))

(ffi:def-function ("_PlaySound" play-sound-raw)
    ((sound (* sound-raw)))
  :returning :void)

(defun play-sound (sound)
  (play-sound-raw (sound-pointer sound)))

(ffi:def-struct music-raw
    (stream audio-stream)
  (frame-count :unsigned-int)
  ;; FIXME: 2025-01-03 This is going to cause trouble.
  (looping :unsigned-byte)
  (ctx-type :int)
  (ctx-data :pointer-void))

(defstruct (music (:constructor @music))
  (pointer nil :type si:foreign-data))

(defmacro music-looping (m)
  `(ffi:get-slot-value (music-pointer ,m) 'music-raw 'looping))

(ffi:def-function ("_LoadMusicStream" load-music-stream-raw)
    ((file-name :cstring))
  :returning (* music-raw))

(defun load-music-stream (file-name)
  (let* ((pointer (load-music-stream-raw (namestring file-name)))
         (music   (@music :pointer pointer)))
    (tg:finalize music (lambda () (free! pointer)))))

(ffi:def-function ("_UnloadMusicStream" unload-music-stream-raw)
    ((music (* music-raw)))
  :returning :void)

(defun unload-music-stream (music)
  (unload-music-stream-raw (music-pointer music)))

(ffi:def-function ("_IsMusicStreamPlaying" is-music-stream-playing-raw)
    ((music (* music-raw)))
  ;; HACK: 2025-01-03 Should be a bool.
  :returning :unsigned-byte)

(defun is-music-stream-playing (music)
  (= 1 (is-music-stream-playing-raw (music-pointer music))))

(ffi:def-function ("_PlayMusicStream" play-music-stream-raw)
    ((music (* music-raw)))
  :returning :void)

(defun play-music-stream (music)
  (play-music-stream-raw (music-pointer music)))

(ffi:def-function ("_UpdateMusicStream" update-music-stream-raw)
    ((music (* music-raw)))
  :returning :void)

(defun update-music-stream (music)
  (update-music-stream-raw (music-pointer music)))

;; --- Camera --- ;;

(ffi:def-struct camera-2d-raw
    (offset vector2-raw)
  (target vector2-raw)
  (rotation :float)
  (zoom :float))

(defstruct (camera-2d (:constructor @camera-2d))
  (pointer nil :type si:foreign-data))

(ffi:def-function ("_MakeCamera2D" make-camera-2d-raw)
    ((offset (* vector2-raw))
     (target (* vector2-raw))
     (rotation :float)
     (zoom :float))
  :returning (* camera-2d-raw))

(defun make-camera-2d (&key offset target rotation zoom)
  (let* ((pointer (make-camera-2d-raw (vector2-pointer offset)
                                      (vector2-pointer target)
                                      rotation zoom))
         (camera (@camera-2d :pointer pointer)))
    (tg:finalize camera (lambda () (free! pointer)))))

;; --- Keyboard and Gamepad --- ;;

(ffi:def-function ("IsKeyPressed" is-key-pressed-raw)
    ((key :int))
  ;; HACK: 2025-01-03 Bool.
  :returning :unsigned-byte)

(defun is-key-pressed (key)
  (= 1 (is-key-pressed-raw key)))

(ffi:def-function ("IsKeyDown" is-key-down-raw)
    ((key :int))
  ;; HACK: 2025-01-03 Bool.
  :returning :unsigned-byte)

(defun is-key-down (key)
  (= 1 (is-key-down-raw key)))

(ffi:def-function ("IsGamepadButtonPressed" is-gamepad-button-pressed-raw)
    ((gamepad :int)
     (key :int))
  ;; HACK: 2025-01-03 Bool.
  :returning :unsigned-byte)

(defun is-gamepad-button-pressed (gamepad key)
  (= 1 (is-gamepad-button-pressed-raw gamepad key)))

(ffi:def-function ("IsGamepadButtonDown" is-gamepad-button-down-raw)
    ((gamepad :int)
     (key :int))
  ;; HACK: 2025-01-03 Bool.
  :returning :unsigned-byte)

(defun is-gamepad-button-down (gamepad key)
  (= 1 (is-gamepad-button-down-raw gamepad key)))

(ffi:def-function ("GetGamepadName" get-gamepad-name)
    ((gamepad :int))
  ;; FIXME: 2025-01-03 Deallocation might become an issue. I could either:
  ;; (1) Avoid exposing this function altogether.
  ;; (2) Just leak the memory since I know this is only called interactively.
  ;; (3) Use a custom wrapper struct whose finalizer I control.
  :returning :cstring)

(ffi:def-function ("IsGamepadAvailable" is-gamepad-available-raw)
    ((gamepad :int))
  ;; HACK: 2025-01-03 Bools again.
  :returning :unsigned-byte)

(defun is-gamepad-available (n)
  (= 1 (is-gamepad-available-raw n)))

(ffi:def-function ("GetGamepadAxisCount" get-gamepad-axis-count)
    ((gamepad :int))
  :returning :int)

(ffi:def-function ("GetGamepadAxisMovement" get-gamepad-axis-movement)
    ((gamepad :int)
     (axis :int))
  :returning :float)

;; --- Window --- ;;

(ffi:def-function ("InitWindow" init-window)
    ((width :int)
     (height :int)
     (title :cstring))
  :returning :void)

(ffi:def-function ("CloseWindow" close-window) () :returning :void)

(ffi:def-function ("InitAudioDevice" init-audio-device) () :returning :void)

(ffi:def-function ("CloseAudioDevice" close-audio-device) () :returning :void)

(ffi:def-function ("SetTargetFPS" set-target-fps)
    ((fps :int))
  :returning :void)

(ffi:def-function ("WindowShouldClose" window-should-close-raw) ()
  ;; HACK: 2025-01-03 Bool.
  :returning :unsigned-byte)

(defun window-should-close ()
  (= 1 (window-should-close-raw)))

(ffi:def-function ("BeginDrawing" begin-drawing) () :returning :void)

(ffi:def-function ("EndDrawing" end-drawing) () :returning :void)

(ffi:def-function ("_BeginMode2D" begin-mode-2d-raw)
    ((camera (* camera-2d-raw)))
  :returning :void)

(defun begin-mode-2d (camera)
  (begin-mode-2d-raw (camera-2d-pointer camera)))

(ffi:def-function ("EndMode2D" end-mode-2d) () :returning :void)

(ffi:def-function ("_ClearBackground" clear-background-raw)
    ((color (* color-raw)))
  :returning :void)

(defun clear-background (color)
  (clear-background-raw (color-pointer color)))

(ffi:def-function ("DrawFPS" draw-fps)
    ((pos-x :int)
     (pos-y :int))
  :returning :void)

(ffi:def-function ("_DrawText" draw-text-raw)
    ((text :cstring)
     (pos-x :int)
     (pos-y :int)
     (font-size :int)
     (color (* color-raw)))
  :returning :void)

(declaim (ftype (function (string fixnum fixnum fixnum color) null) draw-text))
(defun draw-text (text pos-x pos-y font-size color)
  (draw-text-raw text pos-x pos-y font-size (color-pointer color)))

(ffi:def-function ("_DrawRectangle" draw-rectangle-raw)
    ((pos-x :int)
     (pos-y :int)
     (width :int)
     (height :int)
     (color (* color-raw)))
  :returning :void)

(declaim (ftype (function (fixnum fixnum fixnum fixnum color) null) draw-rectangle))
(defun draw-rectangle (pos-x pos-y width height color)
  (draw-rectangle-raw pos-x pos-y width height (color-pointer color)))

(ffi:def-function ("_DrawLine" draw-line-raw)
    ((start-pos-x :int)
     (start-pos-y :int)
     (end-pos-x :int)
     (end-pos-y :int)
     (color (* color-raw)))
  :returning :void)

(declaim (ftype (function (fixnum fixnum fixnum fixnum color) null) draw-line))
(defun draw-line (start-pos-x start-pos-y end-pos-x end-pos-y color)
  (draw-line-raw start-pos-x start-pos-x end-pos-x end-pos-y (color-pointer color)))

(ffi:def-function ("_DrawPixel" draw-pixel-raw)
    ((pos-x :int)
     (pos-y :int)
     (color (* color-raw))))

(declaim (ftype (function (fixnum fixnum color) null) draw-pixel))
(defun draw-pixel (pos-x pos-y color)
  (draw-pixel-raw pos-x pos-x (color-pointer color)))

;; --- Collision --- ;;

(ffi:def-function ("_CheckCollisionRecs" check-collision-recs-raw)
    ((rec1 (* rectangle-raw))
     (rec2 (* rectangle-raw)))
  ;; HACK: 2025-01-03 Bool.
  :returning :unsigned-byte)

(defun check-collision-recs (rec1 rec2)
  (= 1 (check-collision-recs-raw (rectangle-pointer rec1)
                                 (rectangle-pointer rec2))))
