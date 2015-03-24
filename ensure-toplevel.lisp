;;;; ensure-toplevel.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

(defmacro ensure-toplevel (&body body)
  "Ensures that BODY is executed at the top level. Otherwise, an error occurs."
  (let ((sym (gensym "TOPLEVEL-CHECKER-")))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel)
         (setf (symbol-value ',sym) t))
       (eval-when (:execute)
         (unless (boundp ',sym)
           (error "A given form was not found at the top level when it ~
                   was asserted to be. Found:~%~4T~S"
                  '(progn ,@body))))
       ,@body)))

#+#:IGNORE
(progn
  ;; OK
  (ensure-toplevel
    (defparameter *foo* 1))

  ;; OK
  (ensure-toplevel
    (defun f (x)
      (* x x)))

  ;; ERROR
  (let ((counter 1))
    (ensure-toplevel
      (defun g ()
        (incf counter)))))
