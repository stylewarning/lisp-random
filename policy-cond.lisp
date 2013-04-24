;;;; policy-cond.lisp
;;;; Copyright (c) 2013 Robert Smith

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro policy (expr env)
    (let ((policy (sb-cltl2:declaration-information 'optimize env)))
      `(let ,policy
         (declare (ignorable ,@(mapcar #'car policy)))
         ,expr)))

  (defmacro policy-if (expr then else &environment env)
    (if (eval `(policy ,expr ,env))
        then
        else)))

(declaim (optimize (speed 3) (safety 0)))

(defun foo ()
  (policy-if (> speed safety)
             (+ 1 1)
             (* 4 4)))

(declaim (optimize (speed 0) (safety 3)))

(defun bar ()
  (policy-if (> speed safety)
             (+ 1 1)
             (* 4 4)))
