;;;; ski.lisp
;;;; Copyright (c) 2013 Robert Smith

;;;; Implementation of the SKI combinator calculus.

(defun I (x)
  x)

(defun K (x)
  (lambda (y)
    x))

(defun S (x)
  (lambda (y)
    (lambda (z)
      (funcall (funcall x z) (funcall y z)))))
