;;;; fixed-size-stack.lisp
;;;; Copyright (c) 2015 Robert Smith
(defstruct s n c i s)
(defun smake (n) (make-s :n n :c 0 :i 0 :s (make-array n)))
(defun spush (x s) (setf (s-c s) (min (1+ (s-c s)) (s-n s))) (prog1 (setf (aref (s-s s) (s-i s)) x) (setf (s-i s) (mod (1+ (s-i s)) (s-n s)))))
(defun spop (s) (and (plusp (s-c s)) (progn (setf (s-i s) (mod (1- (s-i s)) (s-n s))) (setf (s-c s) (1- (s-c s))) (aref (s-s s) (s-i s)))))
