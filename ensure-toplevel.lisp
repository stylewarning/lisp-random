;;;; ensure-toplevel.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

;;; Bug: This should error at compile time, but currently only errors
;;;      at load time.
(defmacro ensure-toplevel (&body body)
  "Checks that BODY will be executed at the top level. Otherwise, an error occurs."
  (let ((at-toplevel-p (gensym "AT-TOPLEVEL-P-")))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel)
         (setf (symbol-value ',at-toplevel-p) t))
       (eval-when (:execute)
         (load-time-value
          (unless (boundp ',at-toplevel-p)
            (error "A given form was not found at the top level when it ~
                    was asserted to be. Found:~%~4T~S"
                   '(progn ,@body)))))
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
        (incf counter))))
  
  ;; ERROR
  (defun f (x)
    (ensure-toplevel
      (defun g (x)
        (* x x)))))
