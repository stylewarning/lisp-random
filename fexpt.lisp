;;;; fexpt.lisp
;;;;
;;;; Copyright (c) 2018 Robert Smith

;;; EXERCISE: Write a function FEXPT : (a -> a) * Integer -> (a -> a)
;;; which takes a unary function F and a non-negative integer N and
;;; computes a function equivalent to N compositions of F. Write this
;;; purely functionally.

(defun compose (f g)
  (lambda (x) (funcall f (funcall g x))))

(defun fexpt (f n)
  (cond
    ((zerop n) (lambda (x) x))
    ((oddp n) (compose f (fexpt f (1- n))))
    ((evenp n) (let ((f^n/2 (fexpt f (/ n 2))))
                 (compose f^n/2 f^n/2)))))
