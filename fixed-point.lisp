;;;; fixed-point.lisp
;;;; Copyright (c) 2013 Robert Smith

;;;; The fixed-point combinator.

;;; From qtility.
(defun fix (f)
  "Apply the fixed-point combinator, also known as the Y-combinator,
  to the function F : (A -> B) -> A -> B."
  ((lambda (x) (funcall x x))
   (lambda (x) (funcall f (lambda (y)
                            (funcall (funcall x x) y))))))

;; Example invocation.
;;
;; CL-USER> (fix (lambda (f)
;;                 (lambda (x)
;;                   (if (zerop x)
;;                       1
;;                       (* x (funcall f (1- x)))))))
;; #<CLOSURE (LAMBDA (X)) {1006C0186B}>
;; CL-USER> (funcall * 5)
;; 120


(defmacro label (name (&rest args) &body body)
  (let ((arg (gensym "ARG-")))
    `(fix (lambda (,name)
            ;; Allow NAME to be used in function position.
            (flet ((,name (,arg)
                     (funcall ,name ,arg)))
              (lambda ,args
                ,@body))))))

;; Example invocation.
;;
;; CL-USER> (label fact (n) (if (zerop n) 1 (* n (fact (1- n)))))
;; #<CLOSURE (LAMBDA (N)) {100692D01B}>
;; CL-USER> (funcall * 5)
;; 120
