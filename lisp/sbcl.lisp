(in-package :raylib)

;; --- Vectors --- ;;

(define-alien-type nil
    (struct vector2-raw
            (x float)
            (y float)))

(define-alien-routine ("_MakeVector2" make-vector2-raw) (* (struct vector2-raw))
  (x float)
  (y float))

#++
(type-of (make-vector2-raw 1.0 2.0))

(defstruct (vector2 (:constructor @vector2))
  (pointer nil :type (alien (* (struct vector2-raw
                                       (x single-float :offset 0)
                                       (y single-float :offset 32))))))

(declaim (ftype (function (&key (:x real) (:y real)) vector2) make-vector2))
(defun make-vector2 (&key x y)
  (let* ((ptr (make-vector2-raw (float x) (float y)))
         (v   (@vector2 :pointer ptr)))
    (tg:finalize v (lambda () (free-alien ptr)))))

(defmacro vector2-x (v)
  "The X slot of a `Vector2'."
  `(slot (vector2-pointer ,v) 'x))

(defmacro vector2-y (v)
  "The Y slot of a `Vector2'."
  `(slot (vector2-pointer ,v) 'y))

(defmethod print-object ((v vector2) stream)
  (print-unreadable-object (v stream :type t :identity t)
    (format stream ":X ~a :Y ~a" (vector2-x v) (vector2-y v))))

#+slynk
(defmethod slynk:emacs-inspect ((v vector2))
  `("This is an FFI-wrapping of the pointer:"
    (:newline)
    ,(format nil "~a" (vector2-pointer v))
    (:newline)
    (:newline)
    "The struct's underlying values are:"
    (:newline)
    ,(format nil "X: ~a" (vector2-x v))
    (:newline)
    ,(format nil "Y: ~a" (vector2-y v))))

#++
(let ((v (make-vector2 :x 1 :y 2)))
  (setf (vector2-x v) 1000.0)
  (vector2-x v))

#++
(tg:gc :full t :verbose t)

;; --- Rectangles --- ;;

(define-alien-type nil
    (struct rectangle-raw
            (x float)
            (y float)
            (width float)
            (height float)))

(define-alien-routine ("_MakeRectangle" make-rectangle-raw) (* (struct rectangle-raw))
  (x float)
  (y float)
  (width float)
  (height float))

#++
(type-of (make-rectangle-raw 1.0 2.0 16.0 16.0))

(defstruct (rectangle (:constructor @rectangle))
  (pointer nil :type (alien (* (struct rectangle-raw
                                       (x      single-float :offset 0)
                                       (y      single-float :offset 32)
                                       (width  single-float :offset 64)
                                       (height single-float :offset 96))))))

(declaim (ftype (function (&key (:x real) (:y real) (:width real) (:height real)) rectangle) make-rectangle))
(defun make-rectangle (&key x y width height)
  (let* ((pointer (make-rectangle-raw (float x) (float y) (float width) (float height)))
         (rect    (@rectangle :pointer pointer)))
    (tg:finalize rect (lambda () (free-alien pointer)))))

(defmacro rectangle-x (r)
  `(slot (rectangle-pointer ,r) 'x))

(defmacro rectangle-y (r)
  `(slot (rectangle-pointer ,r) 'y))

(defmacro rectangle-width (r)
  `(slot (rectangle-pointer ,r) 'width))

(defmacro rectangle-height (r)
  `(slot (rectangle-pointer ,r) 'height))

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
    ,(format nil "~a" (rectangle-pointer r))
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

;; --- Colour --- ;;

(define-alien-type nil
    (struct color-raw
            (r unsigned-char)
            (g unsigned-char)
            (b unsigned-char)
            (a unsigned-char)))

(define-alien-routine ("_MakeColor" make-color-raw) (* (struct color-raw))
  (r unsigned-char)
  (g unsigned-char)
  (b unsigned-char)
  (a unsigned-char))

;; FIXME: 2025-01-27 Offsets here too?
(defstruct (color (:constructor @color))
  (pointer nil :type alien))

(defun make-color (&key r g b a)
  (let* ((pointer (make-color-raw r g b a))
         (color   (@color :pointer pointer)))
    (tg:finalize color (lambda () (free-alien pointer)))))

(define-alien-routine ("_ColorAlpha" color-alpha-raw) (* (struct color-raw))
  (color (* (struct color-raw)))
  (alpha float))

(defun color-alpha (color alpha)
  (let* ((ptr (color-alpha-raw (color-pointer color) alpha))
         (new (@color :pointer ptr)))
    (tg:finalize new (lambda () (free-alien ptr)))))

(defmethod print-object ((c color) stream)
  (print-unreadable-object (c stream :type t :identity t)
    (format stream ":R ~a :G ~a :B ~a :A ~a"
            (slot (color-pointer c) 'r)
            (slot (color-pointer c) 'g)
            (slot (color-pointer c) 'b)
            (slot (color-pointer c) 'a))))

;; --- Textures --- ;;

(define-alien-type nil
    (struct texture-raw
            (id unsigned-int)
            (width int)
            (height int)
            (mipmaps int)
            (format int)))

;; FIXME: 2025-01-27 Offsets here too?
(defstruct (texture (:constructor @texture))
  (pointer nil :type alien))

(defmethod print-object ((x texture) stream)
  (print-unreadable-object (x stream :type t :identity t)
    (format stream ":ID ~a :WIDTH ~a :HEIGHT ~a :MIPMAPS ~a :FORMAT ~a"
            (slot (texture-pointer x) 'id)
            (slot (texture-pointer x) 'width)
            (slot (texture-pointer x) 'height)
            (slot (texture-pointer x) 'mipmaps)
            (slot (texture-pointer x) 'format))))

(define-alien-routine ("_LoadTexture" load-texture-raw) (* (struct texture-raw))
  (file-name c-string))

(defun load-texture (file-name)
  (let* ((pointer (load-texture-raw (namestring file-name)))
         (texture (@texture :pointer pointer)))
    (tg:finalize texture (lambda () (free-alien pointer)))))

(define-alien-routine ("_UnloadTexture" unload-texture-raw) void
  (texture (* (struct texture-raw))))

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
  `(slot (texture-pointer ,r) 'width))

(defmacro texture-height (r)
  `(slot (texture-pointer ,r) 'height))

(define-alien-routine ("_IsTextureValid" is-texture-valid-raw) (boolean 8)
  (texture (* (struct texture-raw))))

(declaim (ftype (function (texture) boolean) is-texture-valid))
(defun is-texture-valid (texture)
  (is-texture-valid-raw (texture-pointer texture)))

(define-alien-routine ("_DrawTexture" draw-texture-raw) void
  (texture (* (struct texture-raw)))
  (pos-x int)
  (pos-y int)
  (tint (* (struct color-raw))))

(declaim (ftype (function (texture fixnum fixnum color) null) draw-texture))
(defun draw-texture (texture pos-x pos-y tint)
  (draw-texture-raw (texture-pointer texture)
                    pos-x pos-y
                    (color-pointer tint)))

(define-alien-routine ("_DrawTextureV" draw-texture-v-raw) void
  (texture  (* (struct texture-raw)))
  (position (* (struct vector2-raw)))
  (tint     (* (struct color-raw))))

(declaim (ftype (function (texture vector2 color) null) draw-texture-v))
(defun draw-texture-v (texture position tint)
  (draw-texture-v-raw (texture-pointer texture)
                      (vector2-pointer position)
                      (color-pointer tint)))

(define-alien-routine ("_DrawTextureRec" draw-texture-rec-raw) void
  (texture  (* (struct texture-raw)))
  (source   (* (struct rectangle-raw)))
  (position (* (struct vector2-raw)))
  (tint     (* (struct color-raw))))

(declaim (ftype (function (texture rectangle vector2 color) null) draw-texture-rec))
(defun draw-texture-rec (texture source position tint)
  (draw-texture-rec-raw (texture-pointer texture)
                        (rectangle-pointer source)
                        (vector2-pointer position)
                        (color-pointer tint)))

;; --- Sounds and Music --- ;;

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
  (pointer nil :type alien))

(define-alien-routine ("_LoadSound" load-sound-raw) (* (struct sound-raw))
  (file-name c-string))

(defun load-sound (file-name)
  (let* ((pointer (load-sound-raw (namestring file-name)))
         (sound   (@sound :pointer pointer)))
    (tg:finalize sound (lambda () (free-alien pointer)))))

(define-alien-routine ("_UnloadSound" unload-sound-raw) void
  (sound (* (struct sound-raw))))

(defun unload-sound (sound)
  (unload-sound-raw (sound-pointer sound)))

(define-alien-routine ("_PlaySound" play-sound-raw) void
  (sound (* (struct sound-raw))))

(defun play-sound (sound)
  (play-sound-raw (sound-pointer sound)))

(define-alien-type nil
    (struct music-raw
            (stream (struct audio-stream))
            (frame-count unsigned-int)
            (looping (boolean 8))
            (ctx-type int)
            (ctx-data (* t))))

;; FIXME: 2025-01-27 Offsets here too?
(defstruct (music (:constructor @music))
  (pointer nil :type alien))

(defmacro music-looping (m)
  `(slot (music-pointer ,m) 'looping))

(define-alien-routine ("_LoadMusicStream" load-music-stream-raw) (* (struct music-raw))
  (file-name c-string))

(defun load-music-stream (file-name)
  (let* ((pointer (load-music-stream-raw (namestring file-name)))
         (music   (@music :pointer pointer)))
    (tg:finalize music (lambda () (free-alien pointer)))))

(define-alien-routine ("_UnloadMusicStream" unload-music-stream-raw) void
  (music (* (struct music-raw))))

(defun unload-music-stream (music)
  (unload-music-stream-raw (music-pointer music)))

(define-alien-routine ("_IsMusicStreamPlaying" is-music-stream-playing-raw) (boolean 8)
  (music (* (struct music-raw))))

(defun is-music-stream-playing (music)
  (is-music-stream-playing-raw (music-pointer music)))

(define-alien-routine ("_PlayMusicStream" play-music-stream-raw) void
  (music (* (struct music-raw))))

(defun play-music-stream (music)
  (play-music-stream-raw (music-pointer music)))

(define-alien-routine ("_UpdateMusicStream" update-music-stream-raw) void
  (music (* (struct music-raw))))

(defun update-music-stream (music)
  (update-music-stream-raw (music-pointer music)))

;; --- Camera --- ;;

(define-alien-type nil
    (struct camera-2d-raw
            (offset (struct vector2-raw))
            (target (struct vector2-raw))
            (rotation float)
            (zoom float)))

;; FIXME: 2025-01-27 Offsets here too?
(defstruct (camera-2d (:constructor @camera-2d))
  (pointer nil :type alien))

(defmethod print-object ((c camera-2d) stream)
  (print-unreadable-object (c stream :type t :identity t)
    (format stream ":OFFSET ~a :TARGET ~a :ROTATION ~a :ZOOM ~a"
            (camera-2d-offset c)
            (camera-2d-target c)
            (slot (camera-2d-pointer c) 'rotation)
            (slot (camera-2d-pointer c) 'zoom))))

#+slynk
(defmethod slynk:emacs-inspect ((c camera-2d))
  `("This is an FFI-wrapping of the pointer:"
    (:newline)
    ,(format nil "~a" (camera-2d-pointer c))
    (:newline)
    (:newline)
    "The struct's underlying values are:"
    (:newline)
    ,(format nil "OFFSET: ~a" (camera-2d-offset c))
    (:newline)
    ,(format nil "TARGET: ~a" (camera-2d-target c))
    (:newline)
    ,(format nil "ROTATION: ~a" (slot (camera-2d-pointer c) 'rotation))
    (:newline)
    ,(format nil "ZOOM: ~a" (slot (camera-2d-pointer c) 'zoom))))

(define-alien-routine ("_MakeCamera2D" make-camera-2d-raw) (* (struct camera-2d-raw))
  (offset (* (struct vector2-raw)))
  (target (* (struct vector2-raw)))
  (rotation float)
  (zoom float))

(declaim (ftype (function (&key (:offset vector2) (:target vector2) (:rotation real) (:zoom real)) camera-2d) make-camera-2d))
(defun make-camera-2d (&key offset target rotation zoom)
  (let* ((pointer (make-camera-2d-raw (vector2-pointer offset)
                                      (vector2-pointer target)
                                      (float rotation)
                                      (float zoom)))
         (camera (@camera-2d :pointer pointer)))
    (tg:finalize camera (lambda () (free-alien pointer)))))

;; NOTE: 2025-03-04 So far these are the only functions like this, where we need
;; to wrap a pointer but NOT have it cleaned up by `trivial-garbage'. That is
;; because we're only borrowing it in order to do some updates to the underlying
;; vector.
;;
;; Later, if I don't like this struct wrapping overhead, I can write a custom
;; shim function that does the update "inline".
(defun camera-2d-offset (camera)
  (@vector2 :pointer (sap-alien (alien-sap (slot (camera-2d-pointer camera) 'offset))
                                (* (struct vector2-raw)))))

(defun camera-2d-target (camera)
  (@vector2 :pointer (sap-alien (alien-sap (slot (camera-2d-pointer camera) 'target))
                                (* (struct vector2-raw)))))

(define-alien-routine ("_GetWorldToScreen2D" get-world-to-screen-2d-raw) (* (struct vector2-raw))
  (position (* (struct vector2-raw)))
  (camera   (* (struct camera-2d-raw))))

(declaim (ftype (function (vector2 camera-2d) vector2) get-world-to-screen-2d))
(defun get-world-to-screen-2d (position camera)
  "Get the screen space position for a 2d camera world space position."
  (let* ((pointer (get-world-to-screen-2d-raw (vector2-pointer position)
                                              (camera-2d-pointer camera)))
         (vector (@vector2 :pointer pointer)))
    (tg:finalize vector (lambda () (free-alien pointer)))))

;; --- Keyboard and Gamepad --- ;;

(define-alien-routine ("GetGamepadName" get-gamepad-name) c-string
  (gamepad int))

(define-alien-routine ("IsGamepadAvailable" is-gamepad-available) (boolean 8)
  (gamepad int))

;; --- Window --- ;;

(define-alien-routine ("InitWindow" init-window) void
  (width int)
  (height int)
  (title c-string))

(define-alien-routine ("CloseWindow" close-window) void)

(define-alien-routine ("InitAudioDevice" init-audio-device) void)

(define-alien-routine ("CloseAudioDevice" close-audio-device) void)

(define-alien-routine ("WindowShouldClose" window-should-close) (boolean 8))

(define-alien-routine ("BeginDrawing" begin-drawing) void)

(define-alien-routine ("EndDrawing" end-drawing) void)

(define-alien-routine ("_BeginMode2D" begin-mode-2d-raw) void
  (camera (* (struct camera-2d-raw))))

(defun begin-mode-2d (camera)
  (begin-mode-2d-raw (camera-2d-pointer camera)))

(define-alien-routine ("EndMode2D" end-mode-2d) void)

(define-alien-routine ("_ClearBackground" clear-background-raw) void
  (color (* (struct color-raw))))

(defun clear-background (color)
  (clear-background-raw (color-pointer color)))

(define-alien-routine ("DrawFPS" draw-fps) void
  (pos-x int)
  (pos-y int))

(define-alien-routine ("_DrawText" draw-text-raw) void
  (text c-string)
  (pos-x int)
  (pos-y int)
  (font-size int)
  (color (* (struct color-raw))))

(declaim (ftype (function (string fixnum fixnum fixnum color) null) draw-text))
(defun draw-text (text pos-x pos-y font-size color)
  (draw-text-raw text pos-x pos-y font-size (color-pointer color)))

(define-alien-routine ("_DrawRectangle" draw-rectangle-raw) void
  (pos-x int)
  (pos-y int)
  (width int)
  (height int)
  (color (* (struct color-raw))))

(declaim (ftype (function (fixnum fixnum fixnum fixnum color) null) draw-rectangle))
(defun draw-rectangle (pos-x pos-y width height color)
  (draw-rectangle-raw pos-x pos-y width height (color-pointer color)))

(define-alien-routine ("_DrawLine" draw-line-raw) void
  (start-pos-x int)
  (start-pos-y int)
  (end-pos-x int)
  (end-pos-y int)
  (color (* (struct color-raw))))

(declaim (ftype (function (fixnum fixnum fixnum fixnum color) null) draw-line))
(defun draw-line (start-pos-x start-pos-y end-pos-x end-pos-y color)
  (draw-line-raw start-pos-x start-pos-y end-pos-x end-pos-y (color-pointer color)))

;; --- Input --- ;;

(define-alien-routine ("IsKeyPressed" is-key-pressed) (boolean 8)
  "Check if a key has been pressed once."
  (key int))

(define-alien-routine ("IsKeyDown" is-key-down) (boolean 8)
  "Check if a key is being pressed."
  (key int))

(define-alien-routine ("IsGamepadButtonPressed" is-gamepad-button-pressed) (boolean 8)
  "Check if a gamepad button has been pressed once."
  (gamepad int)
  (key int))

(define-alien-routine ("IsGamepadButtonDown" is-gamepad-button-down) (boolean 8)
  "Check if a gamepad button is being pressed."
  (gamepad int)
  (key int))

;; --- Collision --- ;;

(define-alien-routine ("_CheckCollisionRecs" check-collision-recs-raw) (boolean 8)
  (rec1 (* (struct rectangle-raw)))
  (rec2 (* (struct rectangle-raw))))

(declaim (ftype (function (rectangle rectangle) boolean) check-collision-recs))
(defun check-collision-recs (rec1 rec2)
  "Are two rectangles colliding?"
  (check-collision-recs-raw (rectangle-pointer rec1)
                            (rectangle-pointer rec2)))

(define-alien-routine ("_CheckCollisionPointRec" check-collision-point-rec-raw) (boolean 8)
  (point (* (struct vector2-raw)))
  (rec   (* (struct rectangle-raw))))

(declaim (ftype (function (vector2 rectangle) boolean) check-collision-point-rec))
(defun check-collision-point-rec (v r)
  "Is a point inside a rectangle?"
  (check-collision-point-rec-raw (vector2-pointer v) (rectangle-pointer r)))

;; --- Time --- ;;

(define-alien-routine ("SetTargetFPS" set-target-fps) void
  (fps int))

(define-alien-routine ("GetFrameTime" get-frame-time) float
  "Get time in seconds for last frame drawn (delta time).")
