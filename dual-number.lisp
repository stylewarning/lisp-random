;;;; dual-number.lisp
;;;; Copyright (c) 2012 Robert Smith

(defstruct (dual (:conc-name dual.)
                 (:constructor dual (real &optional (epsilon 0))))
  real
  epsilon)

(defmacro with-dual ((a b) dual-number &body body)
  (let ((g (gensym "G-")))
    `(let* ((,g ,dual-number)
            (,a (dual.real ,g))
            (,b (dual.epsilon ,g)))
       ,@body)))

;;; A dual number A+Bε can be algebraically emulated by the following
;;; matrix:
;;; 
;;;         [ A  B ]
;;;         [      ].
;;;         [ 0  B ]
;;; 
;;; Addition and multiplication of dual numbers follows from the rules
;;; of matrix addition and multiplication.

(defvar epsilon (dual 0 1))


