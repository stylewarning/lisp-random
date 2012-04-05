;;;; gcd.lisp
;;;; Copyright (c) 2012 Robert Smith

(defpackage #:gcd
  (:use #:cl #:qtility)
  (:export #:rec-gcd
           #:iter-gcd
           #:binary-gcd
           #:iter-binary-gcd))

(in-package #:gcd)

(defun rec-gcd (u v)
  (if (zerop v)
      u
      (gcd v (mod u v))))

(defun iter-gcd (u v)
  (until (zerop v)
    (shiftf u v (mod u v)))
  u)

(defun binary-gcd (u v)
  (declare (type (integer 0) u v))
  (cond
    ((= u v) u)
    ((zerop u) v)
    ((zerop v) u)
    ((evenp u) (if (oddp v)
                   (binary-gcd (ash u -1) v)
                   (ash (binary-gcd (ash u -1)
                                    (ash v -1)) 1)))
    ((evenp v) (binary-gcd u (ash v -1)))
    ((> u v) (binary-gcd (ash (- u v) -1) v))
    (t (binary-gcd (ash (- v u) -1) u))))

(defun iter-binary-gcd (u v)
  (declare (type (integer 0) u v))
  (let ((shift 0))
    (cond
      ((zerop u) v)
      ((zerop v) u)
      (t (progn
           (while (evenp (logior u v))
             (setf u (ash u -1)
                   v (ash v -1))
             (incf shift))
           
           (while (evenp u)
             (setf u (ash u -1)))
           
           ;; Do one round of the loop that follows.
           (while (evenp v)
             (setf v (ash v -1)))
           
           (when (> u v)
             (rotatef u v))
           
           (decf v u)
           
           ;; Proceed with loop if necessary
           (until (zerop v)
             (while (evenp v)
               (setf v (ash v -1)))
             
             (when (> u v)
               (rotatef u v))
             
             (decf v u))
           
           (ash u shift))))))