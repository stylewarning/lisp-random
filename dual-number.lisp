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

(defun dual-+ (d1 d2)
  (with-dual (a b) d1
    (with-dual (p q) d2
      (dual (+ a p) (+ b q)))))

(defun dual-negate (d)
  (with-dual (a b) d
    (dual (- a) (- b))))

(defun dual-- (d1 d2)
  (with-dual (a b) d1
    (with-dual (p q) d2
      (dual (- a p) (- b q)))))

(defun dual-* (d1 d2)
  (with-dual (a b) d1
    (with-dual (p q) d2
      (dual (* a p)
            (+ (* b p) (* a q))))))

(defun dual-reciprocal (d)
  (with-dual (a b) d
    (dual (/ p)
          (- (/ q p p)))))

(defun dual-/ (d1 d2)
  (with-dual (a b) d1
    (with-dual (p q) d2
      (dual (/ a p)
            (/ (- (* b p) (* a q))
               p p)))))


