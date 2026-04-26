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
