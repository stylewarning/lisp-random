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
        else))
  
  (defmacro policy-cond (&body cases)
    (if (null cases)
        (error "No policy matches.")
        `(policy-if ,(caar cases)
                    (progn ,@(cdar cases))
                    (policy-cond ,@(cdr cases))))))

;; test stuff

(declaim (optimize (speed 3) (safety 0)))

(defun foo ()
  (declare (optimize safety (speed 0)))
  (policy-if (> speed safety)
             (+ 1 1)
             (* 4 4)))

(declaim (optimize (speed 0) (safety 3)))

(defun bar ()
  (declare (optimize debug))
  (policy-if (and (< speed safety)
                  (= debug safety))
             (+ 1 1)
             (* 4 4)))

(defun test-cond ()
  (policy-cond
    ((> speed safety) (+ 1 1))
    ((= speed safety) (+ 2 2))
    ((< speed safety) (+ 3 3))))

(defun test ()
  (assert (equal '(16 2)
                 (list (foo) (bar))))
  (assert (= 6 (test-cond))))

