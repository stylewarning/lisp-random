;;;; random-arithmetic.lisp
;;;;
;;;; Copyright (c) 2018 Robert Smith

(ql:quickload :alexandria)

;;; Challenge 33: http://www.watrophy.com/posts/33-Random-Arithmetic-Problems.html

;;; First we define randomizer functions.

(defun random-number (min max)
  (check-type min number)
  (check-type max number)
  (assert (< min max))
  (+ min (random (1+ (- max min)))))

(defvar *default-minimum-integer* -100)
(defvar *default-maximum-integer* 100)
(defun random-integer (&optional (min *default-minimum-integer*)
                                 (max *default-maximum-integer*))
  (check-type min integer)
  (check-type max integer)
  (random-number min max))

(defvar *default-minimum-float* -100.0)
(defvar *default-maximum-float* 100.0)
(defun random-float (&optional (min *default-minimum-float*)
                               (max *default-maximum-intege*))
  (check-type min float)
  (check-type max float)
  (random-number min max))

;;; Validity checking & expression generation

(defun denominator-invalidp (expr1 expr2)
  (declare (ignore expr1))
  (zerop (eval expr2)))

(defun generate-sqrt (expr)
  (if (minusp (eval expr))
      `(sqrt (- ,expr))
      `(sqrt ,expr)))

(defun generate-power (expr)
  `(expt ,expr ,(random-integer 2 5)))

;;; The operator table has up to three columns:
;;;
;;;    1. [Required] This is either a symbol or a function. If it is a
;;;        symbol, it is the symbolic name to be generated which also
;;;        names a Lisp function. If it is a function, it is a
;;;        function of a number of variables equal to the second
;;;        column, which takes an expression and returns a generated
;;;        expression.
;;;
;;;    2. [Required] The operator's arity.
;;;
;;;    3. [Optional] A function designator of ARITY arguments which
;;;       returns T if the arguments should be regenerated. This can
;;;       be used for detecting erroneous input. By default this is
;;;       (CONSTANTLY NIL).
(defparameter *operator-table*
  (list (list '+                2)
        (list '-                1)
        (list '-                2)
        (list '*                2)
        (list '/                2  'denominator-invalidp)
        (list #'generate-sqrt   1)
        (list #'generate-power  1)))

(defun random-expression (&key (table *operator-table*)
                               (depth 3)
                               (descent-probability 0.9))
  "Create a random expression using the operator table TABLE with a depth of at most 3. DESCENT-PROBABILITY determines the probability of producing a nesting at any given level.

NOTE: The depth of the resulting expression may exceed DEPTH if the TABLE has entries which themselves increase the depth."
  (check-type depth unsigned-byte)
  (check-type descent-probability (real 0 1))

  (labels ((descend (depth)
             (assert (plusp depth))
             (if (< (random 1.0) descent-probability)
                 (build-expr (1- depth))
                 (random-integer)))
           (generate-arguments (arity depth retry)
             (let ((arguments (loop :repeat arity
                                    :collect (descend depth))))
               (if (apply retry arguments)
                   (generate-arguments arity depth retry)
                   arguments)))
           (build-expr (depth)
             (if (zerop depth)
                 (random-integer)
                 (destructuring-bind (op arity &optional (invalidp (constantly nil)))
                     (alexandria:random-elt table)
                   (let ((arguments (generate-arguments arity depth invalidp)))
                     (etypecase op
                       (symbol `(,op ,@arguments))
                       (function (apply op arguments))))))))
    (let ((expr (build-expr depth)))
      (values expr (eval expr)))))
