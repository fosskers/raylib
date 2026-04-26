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
