;;;; dual-number.lisp
;;;; Copyright (c) 2012 Robert Smith

;;;; TODO
;;;;
;;;;  * Experiment with symbolic functions and dual functions.
;;;;
;;;;  * Allow the definition of multi-argument dual functions,
;;;;    e.g. exponentiation.
;;;;
;;;;  * Look into computing gradients instead of one-dimensional
;;;;    derivatives for automatic differentiation.

;;; Requires MOP.

#-closer-mop
(ql:quickload "CLOSER-MOP")

;;; Structure

(defstruct (dual (:conc-name dual.)
                 (:constructor dual (real &optional (epsilon 0)))
                 (:predicate dualp)
                 (:print-function
                  (lambda (obj str depth)
                    (declare (ignore depth))
                    (print-unreadable-object (obj str)
                      (format str "~A ~:[+~;-~] ~Aε"
                              (dual.real obj)
                              (minusp (dual.epsilon obj))
                              (dual.epsilon obj))))))
  real
  epsilon)

(defun ensure-dual (x)
  (etypecase x
    (real (dual x))
    (dual x)))

(defmacro with-dual ((a b) dual-number &body body)
  (let ((g (gensym "G-")))
    `(let* ((,g (ensure-dual ,dual-number))
            (,a (dual.real ,g))
            (,b (dual.epsilon ,g)))
       ,@body)))

;;; A dual number A+Bε can be algebraically emulated by the following
;;; matrix:
;;; 
;;;         [ A  B ]
;;;         [      ].
;;;         [ 0  A ]
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
    (dual (/ a)
          (- (/ b a a)))))

(defun dual-/ (d1 d2)
  (with-dual (a b) d1
    (with-dual (p q) d2
      (dual (/ a p)
            (/ (- (* b p) (* a q))
               p p)))))

;;; A unary function f : R → R can be lifted to duals by defining f as
;;; 
;;;        f(a + bε) = f(a) + b·f′(a)ε.
;;; 
;;; We will call (f, f′) pairs "dual functions".

(defclass dual-function ()
  ((function :initarg :function
             :accessor dual-function.function)
   (derivative :initarg :derivative
               :accessor dual-function.derivative))
  (:metaclass c2mop:funcallable-standard-class))

(defmethod initialize-instance :after ((df dual-function) &key)
  (with-slots (function derivative) df
    (c2mop:set-funcallable-instance-function
     df
     (lambda (dual-number)
       (with-dual (a b) dual-number
         (dual (funcall function a)
               (* b (funcall derivative a))))))))

(defmacro define-dual-function (name (argument) &key function derivative)
  (check-type name     symbol)
  (check-type argument symbol)
  `(progn
     (setf (symbol-function ',name)
           (make-instance 'dual-function
                          :function (lambda (,argument) ,function)
                          :derivative (lambda (,argument) ,derivative)))
     ',name))


;;; Some dual functions.

(define-dual-function dual-sin (x)
  :function (sin x)
  :derivative (cos x))

(define-dual-function dual-cos (x)
  :function (cos x)
  :derivative (- (sin x)))

(define-dual-function dual-tan (x)
  :function (tan x)
  :derivative (expt (cos x) -2))        ; sec(x)²

(define-dual-function dual-sqrt (x)
  :function (sqrt x)
  :derivative (/ 1 (sqrt x) 2))

(define-dual-function dual-exp (x)
  :function (exp x)
  :derivative (exp x))

(defun dual-expt (dual-number n)
  (check-type n real)

  (with-dual (a b) dual-number
      (if (zerop n)
          (dual 1 b)
          (dual (expt a n)
                (* b n (expt a (1- n))))))) ; aⁿ + b·naⁿ⁻¹ε
