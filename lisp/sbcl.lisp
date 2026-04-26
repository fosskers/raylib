;; NOTE: 2026-04-26 In reworking this module to support struct-by-value passing,
;; I briefly considered not wrapping the underlying C structures and simply
;; yielding the raw Alien values from functions like `make-vector2'. I decided
;; against this though, as downstream consumers (myself included!) of the
;; library may be depending on the wrapper type, for instance for `defmethod'
;; dispatch. While the extra struct wrapping might technically no longer be
;; necessary, it's still the more "polite" option in this case. Nobody enjoys
;; handling raw FFI types.

(in-package :raylib)

;; --- Vectors --- ;;

(define-alien-type nil
    (struct vector2-raw
            (x float)
            (y float)))

(define-alien-routine ("_MakeVector2" make-vector2-raw) (struct vector2-raw)
  (x float)
  (y float))

#+nil
(type-of (make-vector2-raw 1.0 2.0))
#+nil
(make-vector2-raw 1.0 2.0)

(defstruct (vector2 (:constructor @vector2))
  (raw nil :type (alien (struct vector2-raw
                                (x single-float :offset 0)
                                (y single-float :offset 32)))))

(declaim (ftype (function (&key (:x real) (:y real)) vector2) make-vector2))
(defun make-vector2 (&key x y)
  (@vector2 :raw (make-vector2-raw (float x) (float y))))

#+nil
(make-vector2 :x 1 :y 2)

(defmacro vector2-x (v)
  "The X slot of a `Vector2'."
  `(slot (vector2-raw ,v) 'x))

(defmacro vector2-y (v)
  "The Y slot of a `Vector2'."
  `(slot (vector2-raw ,v) 'y))

(defmethod print-object ((v vector2) stream)
  (print-unreadable-object (v stream :type t :identity t)
    (format stream ":X ~a :Y ~a" (vector2-x v) (vector2-y v))))

#+slynk
(defmethod slynk:emacs-inspect ((v vector2))
  `("This is an FFI-wrapping of the pointer:"
    (:newline)
    ,(format nil "~a" (vector2-raw v))
    (:newline)
    (:newline)
    "The struct's underlying values are:"
    (:newline)
    ,(format nil "X: ~a" (vector2-x v))
    (:newline)
    ,(format nil "Y: ~a" (vector2-y v))))

#+nil
(let ((v (make-vector2 :x 1 :y 2)))
  (setf (vector2-x v) 1000.0)
  (vector2-x v))

;; --- Rectangles --- ;;

(define-alien-type nil
    (struct rectangle-raw
            (x float)
            (y float)
            (width float)
            (height float)))

(define-alien-routine ("_MakeRectangle" make-rectangle-raw) (struct rectangle-raw)
  (x float)
  (y float)
  (width float)
  (height float))

#+nil
(type-of (make-rectangle-raw 1.0 2.0 16.0 16.0))

(defstruct (rectangle (:constructor @rectangle))
  (raw nil :type (alien (struct rectangle-raw
                                (x      single-float :offset 0)
                                (y      single-float :offset 32)
                                (width  single-float :offset 64)
                                (height single-float :offset 96)))))

(declaim (ftype (function (&key (:x real) (:y real) (:width real) (:height real)) rectangle) make-rectangle))
(defun make-rectangle (&key x y width height)
  (@rectangle :raw (make-rectangle-raw (float x) (float y) (float width) (float height))))

(defmacro rectangle-x (r)
  `(slot (rectangle-raw ,r) 'x))

(defmacro rectangle-y (r)
  `(slot (rectangle-raw ,r) 'y))

(defmacro rectangle-width (r)
  `(slot (rectangle-raw ,r) 'width))

(defmacro rectangle-height (r)
  `(slot (rectangle-raw ,r) 'height))

(defmethod print-object ((r rectangle) stream)
  (print-unreadable-object (r stream :type t :identity t)
    (format stream ":X ~a :Y ~a :WIDTH ~a :HEIGHT ~a"
            (rectangle-x r)
            (rectangle-y r)
            (rectangle-width r)
            (rectangle-height r))))

#+slynk
(defmethod slynk:emacs-inspect ((r rectangle))
  `("This is an FFI-wrapping of the pointer:"
    (:newline)
    ,(format nil "~a" (rectangle-raw r))
    (:newline)
    (:newline)
    "The struct's underlying values are:"
    (:newline)
    ,(format nil "X: ~a" (rectangle-x r))
    (:newline)
    ,(format nil "Y: ~a" (rectangle-y r))
    (:newline)
    ,(format nil "WIDTH: ~a" (rectangle-width r))
    (:newline)
    ,(format nil "HEIGHT: ~a" (rectangle-height r))))

#+nil
(make-rectangle :x 1 :y 2 :width 3 :height 4)

;; --- Colour --- ;;

(define-alien-type nil
    (struct color-raw
            (r unsigned-char)
            (g unsigned-char)
            (b unsigned-char)
            (a unsigned-char)))

(define-alien-routine ("_MakeColor" make-color-raw) (struct color-raw)
  (r unsigned-char)
  (g unsigned-char)
  (b unsigned-char)
  (a unsigned-char))

(defstruct (color (:constructor @color))
  (raw nil :type (alien (struct color-raw
                                (r unsigned-char :offset 0)
                                (g unsigned-char :offset 8)
                                (b unsigned-char :offset 16)
                                (a unsigned-char :offset 24)))))

(defun make-color (&key r g b a)
  (@color :raw (make-color-raw r g b a)))

(define-alien-routine ("ColorAlpha" color-alpha-raw) (struct color-raw)
  (color (struct color-raw))
  (alpha float))

(defun color-alpha (color alpha)
  (@color :raw (color-alpha-raw (color-raw color) alpha)))

(defmethod print-object ((c color) stream)
  (print-unreadable-object (c stream :type t :identity t)
    (format stream ":R ~a :G ~a :B ~a :A ~a"
            (slot (color-raw c) 'r)
            (slot (color-raw c) 'g)
            (slot (color-raw c) 'b)
            (slot (color-raw c) 'a))))

#+nil
(make-color :r 0 :g 0 :b 0 :a 0)

;; --- Textures --- ;;

(define-alien-type nil
    (struct texture-raw
            (id unsigned-int)
            (width int)
            (height int)
            (mipmaps int)
            (format int)))

(defstruct (texture (:constructor @texture))
  (raw nil :type (alien (struct texture-raw
                                (id unsigned-int :offset 0)
                                (width   int :offset 32)
                                (height  int :offset 64)
                                (mipmaps int :offset 96)
                                (format  int :offset 128)))))

(defmacro texture-width (r)
  `(slot (texture-raw ,r) 'width))

(defmacro texture-height (r)
  `(slot (texture-raw ,r) 'height))

(defmethod print-object ((x texture) stream)
  (print-unreadable-object (x stream :type t :identity t)
    (format stream ":ID ~a :WIDTH ~a :HEIGHT ~a :MIPMAPS ~a :FORMAT ~a"
            (slot (texture-raw x) 'id)
            (slot (texture-raw x) 'width)
            (slot (texture-raw x) 'height)
            (slot (texture-raw x) 'mipmaps)
            (slot (texture-raw x) 'format))))

(define-alien-routine ("LoadTexture" load-texture-raw) (struct texture-raw)
  (file-name c-string))

(defun load-texture (file-name)
  (@texture :raw (load-texture-raw (namestring file-name))))

(define-alien-routine ("UnloadTexture" unload-texture-raw) void
  (texture (struct texture-raw)))

(defun unload-texture (texture)
  (unload-texture-raw (texture-raw texture)))

#+nil
(progn
  (init-window 300 300 "hello!")
  (let ((p (load-texture-raw "test.png")))
    (format t "~a~%" (slot p 'id))
    (format t "~a~%" (slot p 'width))
    (format t "~a~%" (slot p 'height)))
  (close-window))

(define-alien-routine ("IsTextureValid" is-texture-valid-raw) (boolean 8)
  (texture (struct texture-raw)))

(declaim (ftype (function (texture) boolean) is-texture-valid))
(defun is-texture-valid (texture)
  "Did a given `texture' load correctly?"
  (is-texture-valid-raw (texture-raw texture)))

(define-alien-routine ("DrawTexture" draw-texture-raw) void
  (texture (struct texture-raw))
  (pos-x int)
  (pos-y int)
  (tint (struct color-raw)))

(declaim (ftype (function (texture fixnum fixnum color) null) draw-texture))
(defun draw-texture (texture pos-x pos-y tint)
  (draw-texture-raw (texture-raw texture)
                    pos-x pos-y
                    (color-raw tint)))

(define-alien-routine ("DrawTextureV" draw-texture-v-raw) void
  (texture  (struct texture-raw))
  (position (struct vector2-raw))
  (tint     (struct color-raw)))

(declaim (ftype (function (texture vector2 color) null) draw-texture-v))
(defun draw-texture-v (texture position tint)
  (draw-texture-v-raw (texture-raw texture)
                      (vector2-raw position)
                      (color-raw tint)))

(define-alien-routine ("DrawTextureRec" draw-texture-rec-raw) void
  (texture  (struct texture-raw))
  (source   (struct rectangle-raw))
  (position (struct vector2-raw))
  (tint     (struct color-raw)))

(declaim (ftype (function (texture rectangle vector2 color) null) draw-texture-rec))
(defun draw-texture-rec (texture source position tint)
  (draw-texture-rec-raw (texture-raw texture)
                        (rectangle-raw source)
                        (vector2-raw position)
                        (color-raw tint)))

;; --- Fonts --- ;;

(define-alien-type nil
    (struct font-raw
            (base-size int)
            (glyph-count int)
            (glyph-padding int)
            (texture (struct texture-raw))
            (recs (* t))
            (glyphs (* t))))

;; FIXME: 2026-04-26 Offsets?
(defstruct (font (:constructor @font))
  (raw nil :type (alien (struct font-raw))))

(define-alien-routine ("GetFontDefault" get-font-default-raw) (struct font-raw))

(declaim (ftype (function () font) get-font-default))
(defun get-font-default ()
  "Get the default Raylib font."
  (@font :raw (get-font-default-raw)))

(define-alien-routine ("LoadFont" load-font-raw) (struct font-raw)
  (file-name c-string))

(declaim (ftype (function ((or string pathname)) font) load-font))
(defun load-font (file-name)
  "Load a font from a file into GPU memory."
  (@font :raw (load-font-raw (namestring file-name))))

(define-alien-routine ("LoadFontEx" load-font-ex-raw) (struct font-raw)
  (file-name c-string)
  (font-size int)
  (codepoints (* int))
  (codepoint-count int))

(declaim (ftype (function ((or string pathname) fixnum (vector integer)) font) load-font-ex))
(defun load-font-ex (file-name font-size codepoints)
  "Load a font with additional parameters, particularly those pertaining to UTF-8
codepoints. The `codepoints' vector should contain the result of `char-code' for
every character you intend to print in your game."
  (let* ((count (length codepoints))
         (array (make-alien int count)))
    ;; FIXME: 2025-08-07 There may be a way to avoid this manual copy.
    (loop :for i fixnum :from 0 :below count
          :do (setf (deref array i) (aref codepoints i)))
    (let ((font (@font :raw (load-font-ex-raw (namestring file-name) font-size array count))))
      (free-alien array)
      font)))

(define-alien-routine ("UnloadFont" unload-font-raw) void
  (font (struct font-raw)))

(declaim (ftype (function (font)) unload-font))
(defun unload-font (font)
  "Unload a font from GPU memory."
  (unload-font-raw (font-raw font)))

(define-alien-routine ("IsFontValid" is-font-valid-raw) (boolean 8)
  (font (struct font-raw)))

(declaim (ftype (function (font) boolean) is-font-valid))
(defun is-font-valid (font)
  "Did a given `font' load correctly?"
  (is-font-valid-raw (font-raw font)))

;; --- Sounds --- ;;

(define-alien-type nil
    (struct audio-stream
            (buffer (* t))
            (processor (* t))
            (sample-rate unsigned-int)
            (sample-size unsigned-int)
            (channels unsigned-int)))

(define-alien-type nil
    (struct sound-raw
            (stream (struct audio-stream))
            (frame-count unsigned-int)))

;; FIXME: 2025-01-27 Offsets here too?
(defstruct (sound (:constructor @sound))
  (raw nil :type (alien (struct sound-raw))))

(define-alien-routine ("LoadSound" load-sound-raw) (struct sound-raw)
  (file-name c-string))

(defun load-sound (file-name)
  (@sound :raw (load-sound-raw (namestring file-name))))

(define-alien-routine ("UnloadSound" unload-sound-raw) void
  (sound (struct sound-raw)))

(defun unload-sound (sound)
  (unload-sound-raw (sound-raw sound)))

(define-alien-routine ("PlaySound" play-sound-raw) void
  (sound (struct sound-raw)))

(defun play-sound (sound)
  (play-sound-raw (sound-raw sound)))

;; --- Music --- ;;

(define-alien-type nil
    (struct music-raw
            (stream (struct audio-stream))
            (frame-count unsigned-int)
            (looping (boolean 8))
            (ctx-type int)
            (ctx-data (* t))))

;; FIXME: 2025-01-27 Offsets here too?
(defstruct (music (:constructor @music))
  (raw nil :type (alien (struct music-raw))))

(defmacro music-looping (m)
  `(slot (music-raw ,m) 'looping))

(define-alien-routine ("LoadMusicStream" load-music-stream-raw) (struct music-raw)
  (file-name c-string))

(defun load-music-stream (file-name)
  (@music :raw (load-music-stream-raw (namestring file-name))))

(define-alien-routine ("UnloadMusicStream" unload-music-stream-raw) void
  (music (struct music-raw)))

(defun unload-music-stream (music)
  (unload-music-stream-raw (music-raw music)))

(define-alien-routine ("IsMusicStreamPlaying" is-music-stream-playing-raw) (boolean 8)
  (music (struct music-raw)))

(defun is-music-stream-playing (music)
  (is-music-stream-playing-raw (music-raw music)))

(define-alien-routine ("PlayMusicStream" play-music-stream-raw) void
  (music (struct music-raw)))

(defun play-music-stream (music)
  (play-music-stream-raw (music-raw music)))

(define-alien-routine ("UpdateMusicStream" update-music-stream-raw) void
  (music (struct music-raw)))

(defun update-music-stream (music)
  (update-music-stream-raw (music-raw music)))

;; --- Camera --- ;;

(define-alien-type nil
    (struct camera-2d-raw
            (offset (struct vector2-raw))
            (target (struct vector2-raw))
            (rotation float)
            (zoom float)))

;; FIXME: 2025-01-27 Offsets here too?
(defstruct (camera-2d (:constructor @camera-2d))
  (raw nil :type (alien (struct camera-2d-raw))))

(defmethod print-object ((c camera-2d) stream)
  (print-unreadable-object (c stream :type t :identity t)
    (format stream ":OFFSET ~a :TARGET ~a :ROTATION ~a :ZOOM ~a"
            (camera-2d-offset c)
            (camera-2d-target c)
            (slot (camera-2d-raw c) 'rotation)
            (slot (camera-2d-raw c) 'zoom))))

#+slynk
(defmethod slynk:emacs-inspect ((c camera-2d))
  `("This is an FFI-wrapping of the pointer:"
    (:newline)
    ,(format nil "~a" (camera-2d-raw c))
    (:newline)
    (:newline)
    "The struct's underlying values are:"
    (:newline)
    ,(format nil "OFFSET: ~a" (camera-2d-offset c))
    (:newline)
    ,(format nil "TARGET: ~a" (camera-2d-target c))
    (:newline)
    ,(format nil "ROTATION: ~a" (slot (camera-2d-raw c) 'rotation))
    (:newline)
    ,(format nil "ZOOM: ~a" (slot (camera-2d-raw c) 'zoom))))

(define-alien-routine ("MakeCamera2D" make-camera-2d-raw) (struct camera-2d-raw)
  (offset (struct vector2-raw))
  (target (struct vector2-raw))
  (rotation float)
  (zoom float))

(declaim (ftype (function (&key (:offset vector2) (:target vector2) (:rotation real) (:zoom real)) camera-2d) make-camera-2d))
(defun make-camera-2d (&key offset target rotation zoom)
  (@camera-2d :raw (make-camera-2d-raw (vector2-raw offset)
                                       (vector2-raw target)
                                       (float rotation)
                                       (float zoom))))

;; NOTE: 2026-04-26 This extra wrapping is still wasteful.
(defun camera-2d-offset (camera)
  (@vector2 :raw (slot (camera-2d-raw camera) 'offset)))

(defun camera-2d-target (camera)
  (@vector2 :raw (slot (camera-2d-raw camera) 'target)))

(define-alien-routine ("GetWorldToScreen2D" get-world-to-screen-2d-raw) (struct vector2-raw)
  (position (struct vector2-raw))
  (camera   (struct camera-2d-raw)))

(declaim (ftype (function (vector2 camera-2d) vector2) get-world-to-screen-2d))
(defun get-world-to-screen-2d (position camera)
  "Get the screen space position for a 2d camera world space position."
  (@vector2 :raw (get-world-to-screen-2d-raw (vector2-raw position)
                                             (camera-2d-raw camera))))

;; --- Keyboard and Gamepad --- ;;

(define-alien-routine ("IsKeyPressed" is-key-pressed) (boolean 8)
  "Check if a key has been pressed once."
  (key int))

(define-alien-routine ("IsKeyDown" is-key-down) (boolean 8)
  "Check if a key is being pressed."
  (key int))

(define-alien-routine ("GetGamepadName" get-gamepad-name) c-string
  "The self-reported name of the given gamepad."
  (gamepad int))

(define-alien-routine ("IsGamepadAvailable" is-gamepad-available) (boolean 8)
  "Whether a particular gamepad ID corresponds to a usable gamepad."
  (gamepad int))

(define-alien-routine ("GetGamepadButtonPressed" get-gamepad-button-pressed) int
  "The currently pressed gamepad button.")

(define-alien-routine ("IsGamepadButtonPressed" is-gamepad-button-pressed) (boolean 8)
  "Check if a gamepad button has been pressed once."
  (gamepad int)
  (key int))

(define-alien-routine ("IsGamepadButtonDown" is-gamepad-button-down) (boolean 8)
  "Check if a gamepad button is being pressed."
  (gamepad int)
  (key int))

(define-alien-routine ("GetGamepadAxisCount" get-gamepad-axis-count) int
  "The number of stick axes a gamepad has. Each stick has two."
  (gamepad int))

(define-alien-routine ("GetGamepadAxisMovement" get-gamepad-axis-movement) float
  "A value from -1 to 1."
  (gamepad int)
  (axis int))

;; --- Window --- ;;

(define-alien-routine ("InitWindow" init-window) void
  (width int)
  (height int)
  (title c-string))

(define-alien-routine ("CloseWindow" close-window) void)

(define-alien-routine ("InitAudioDevice" init-audio-device) void)

(define-alien-routine ("CloseAudioDevice" close-audio-device) void)

(define-alien-routine ("WindowShouldClose" window-should-close) (boolean 8))

;; --- Drawing --- ;;

(define-alien-routine ("BeginDrawing" begin-drawing) void)

(define-alien-routine ("EndDrawing" end-drawing) void)

(define-alien-routine ("BeginMode2D" begin-mode-2d-raw) void
  (camera (struct camera-2d-raw)))

(defun begin-mode-2d (camera)
  (begin-mode-2d-raw (camera-2d-raw camera)))

(define-alien-routine ("EndMode2D" end-mode-2d) void)

(define-alien-routine ("ClearBackground" clear-background-raw) void
  (color (struct color-raw)))

(defun clear-background (color)
  (clear-background-raw (color-raw color)))

(define-alien-routine ("DrawFPS" draw-fps) void
  (pos-x int)
  (pos-y int))

(define-alien-routine ("DrawText" draw-text-raw) void
  (text c-string)
  (pos-x int)
  (pos-y int)
  (font-size int)
  (color (struct color-raw)))

(declaim (ftype (function (string fixnum fixnum fixnum color) null) draw-text))
(defun draw-text (text pos-x pos-y font-size color)
  "Draw text using the default font."
  (draw-text-raw text pos-x pos-y font-size (color-raw color)))

(define-alien-routine ("DrawTextEx" draw-text-ex-raw) void
  (font (struct font-raw))
  (text c-string)
  (position (struct vector2-raw))
  (font-size single-float)
  (spacing single-float)
  (tint (struct color-raw)))

(declaim (ftype (function (font string vector2 real real color)) draw-text-ex))
(defun draw-text-ex (font text position font-size spacing tint)
  "Draw text using a `font' and additional parameters."
  (draw-text-ex-raw (font-raw font) text (vector2-raw position) (float font-size) (float spacing) (color-raw tint)))

(define-alien-routine ("DrawRectangle" draw-rectangle-raw) void
  (pos-x int)
  (pos-y int)
  (width int)
  (height int)
  (color (struct color-raw)))

(declaim (ftype (function (fixnum fixnum fixnum fixnum color) null) draw-rectangle))
(defun draw-rectangle (pos-x pos-y width height color)
  (draw-rectangle-raw pos-x pos-y width height (color-raw color)))

(define-alien-routine ("DrawLine" draw-line-raw) void
  (start-pos-x int)
  (start-pos-y int)
  (end-pos-x int)
  (end-pos-y int)
  (color (struct color-raw)))

(declaim (ftype (function (fixnum fixnum fixnum fixnum color) null) draw-line))
(defun draw-line (start-pos-x start-pos-y end-pos-x end-pos-y color)
  (draw-line-raw start-pos-x start-pos-y end-pos-x end-pos-y (color-raw color)))

(define-alien-routine ("DrawPixel" draw-pixel-raw) void
  (pos-x int)
  (pos-y int)
  (color (struct color-raw)))

(declaim (ftype (function (fixnum fixnum color) null) draw-pixel))
(defun draw-pixel (pos-x pos-y color)
  (draw-pixel-raw pos-x pos-y (color-raw color)))

;; --- Collision --- ;;

(define-alien-routine ("CheckCollisionRecs" check-collision-recs-raw) (boolean 8)
  (rec1 (struct rectangle-raw))
  (rec2 (struct rectangle-raw)))

(declaim (ftype (function (rectangle rectangle) boolean) check-collision-recs))
(defun check-collision-recs (rec1 rec2)
  "Are two rectangles colliding?"
  (check-collision-recs-raw (rectangle-raw rec1)
                            (rectangle-raw rec2)))

(define-alien-routine ("CheckCollisionPointRec" check-collision-point-rec-raw) (boolean 8)
  (point (struct vector2-raw))
  (rec   (struct rectangle-raw)))

(declaim (ftype (function (vector2 rectangle) boolean) check-collision-point-rec))
(defun check-collision-point-rec (v r)
  "Is a point inside a rectangle?"
  (check-collision-point-rec-raw (vector2-raw v) (rectangle-raw r)))
