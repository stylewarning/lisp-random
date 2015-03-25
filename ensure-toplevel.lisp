;;;; ensure-toplevel.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

;;; Thanks to Bike from IRC. He thought of the critical ideas to make
;;; this work correctly.

(defmacro assert-toplevel (&body body)
  "Checks that BODY will be executed at the top level. Otherwise, an error occurs."
  (let ((at-toplevel-p (gensym "AT-TOPLEVEL-P-")))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel)
         (setf (symbol-value ',at-toplevel-p) t))
       (eval-when (:execute)
         (macrolet ((compile-time-check ()
                      (unless (boundp ',at-toplevel-p)
                        (cerror "Go ahead anyway."
                                "A given form was not found at the top level ~
                                when it was asserted to be. Found:~%~4T~S"
                               '(progn ,@body)))))
           (compile-time-check)))
       ,@body)))

#+#:IGNORE
(progn
  ;; OK
  (assert-toplevel
    (defparameter *foo* 1))

  ;; OK
  (assert-toplevel
    (defun f (x)
      (* x x)))

  ;; ERROR
  (let ((counter 1))
    (assert-toplevel
      (defun g ()
        (incf counter))))
  
  ;; ERROR
  (defun f (x)
    (assert-toplevel
      (defun g (x)
        (* x x)))))
