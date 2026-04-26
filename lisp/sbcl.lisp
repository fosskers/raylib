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
