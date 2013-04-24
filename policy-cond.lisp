;;;; policy-cond.lisp
;;;; Copyright (c) 2013 Robert Smith

;;;; DOES NOT WORK

(defun same-name (s1 s2)
  (string= (symbol-name s1)
           (symbol-name s2)))

(defun policy (env)
  (sb-cltl2:declaration-information 'optimize env))

(defmacro policy-if (policy-expression then else &environment env)
  (if (funcall (compile nil `(lambda ()
                               (let ((policy (policy env)))
                                 `(let ,policy
                                    (declare (ignorable ,@(mapcar #'car policy)))
                                    ,policy-expression)))))
      then
      else))

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
