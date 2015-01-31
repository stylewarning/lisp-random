;;;; list-comprehension.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

(defpackage #:list-comprehensions
  (:nicknames #:lc)
  (:use #:cl)
  (:export #:lc
           #:define-lc-clause
           #:continue))

(in-package #:list-comprehensions)

(defmacro with-collector ((var collector) &body body)
  "Execute BODY with COLLECTOR fbound to a function which will collect its argument to VAR efficiently. VAR will be bound to the list of collected values.

Mutating the list VAR or calling COLLECTOR on VAR has undefined behavior."
  (check-type var symbol)
  (check-type collector symbol)
  (let ((tail     (gensym "TAIL-"))
        (new-tail (gensym "NEW-TAIL-"))
        (value    (gensym "VALUE-")))
    `(let ((,var nil)
           (,tail nil))
       (flet ((,collector (,value)
                #+#:sanity-check
                (unless (eq ,value ,var)
                  (error "Cannot collect the list ~S being collected to." ',var))
                (let ((,new-tail (cons ,value nil)))
                  (if (null ,var)
                      (setf ,var  ,new-tail)
                      (setf (cdr ,tail) ,new-tail))
                  (setf ,tail ,new-tail)
                  ;; Return the value.
                  ,value)))
         ,@body))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun kw (symb)
    (intern (symbol-name symb) :keyword)))

(defvar *lc-clause-expanders* (make-hash-table :test 'eq))

(defmacro define-lc-clause (name lambda-list &body body)
  "Define a new list comprehension clause named NAME which takes the arguments as specified by the ordinary lambda list LAMBDA-LIST, executing BODY. BODY should have a form similar to that of a macro: a quasiquoted form representing the expansion of the clause.

Within BODY, a local function `CONTINUE' should be called which will expand into the remaining code of the list comprehension."
  (let ((name (kw name))
        (continue (gensym "CONTINUE-"))
        (clause-arguments (gensym "CLAUSE-ARGUMENTS-")))
    `(progn
       (setf (gethash ,name *lc-clause-expanders*)
             (lambda (,clause-arguments ,continue)
               (flet ((continue ()
                        (funcall ,continue)))
                 (destructuring-bind ,lambda-list
                     ,clause-arguments
                   ,@body))))
       ,name)))

(define-lc-clause if (&rest items)
  `(when (and ,@items)
     ,(continue)))

(define-lc-clause with (var val)
  `(let ((,var ,val))
     ,(continue)))

(define-lc-clause in (var seq)
  `(map nil
        (lambda (,var) ,(continue))
        ,seq))

(define-lc-clause for (var min max)
  `(loop :for ,var :from ,min :below ,max
         :do (progn ,(continue))))

(defmacro lc (expr &rest clauses)
  "Compute the list comprehension of the expression EXPR with the clauses CLAUSES.

By default, the following clause types are supported:

    (IF <item>*)

        Only includes EXPR if every one of <item> expressions are satisfied.

    (WITH <var> <val>)

        Binds the variable <var> to the value <val> in the remaining clauses as well as in EXPR.

    (FOR <var> <min> <max>)

        Iterates <var> from the value <min> to below <max>.

    (IN <var> <seq>)

        Iterates <var> through each item of the sequence <seq>.

New clauses can be defined with the macro `DEFINE-LC-CLAUSE'."
  (let ((result (gensym "RESULT-"))
        (collect (gensym "COLLECT-")))
    (labels ((process-clauses (clauses)
               (if (null clauses)
                   `(,collect ,expr)
                   (destructuring-bind (type &rest clause-items)
                       (car clauses)
                     (funcall
                      (gethash (kw type) *lc-clause-expanders*)
                      clause-items
                      (lambda ()
                        (process-clauses (cdr clauses))))))))
      `(with-collector (,result ,collect)
         ,(process-clauses clauses)
         ,result))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Examples ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cartesian-product (list1 list2)
  (lc (list x y) (for x list1) (for y list2)))

;;; CL-USER> (cartesian-product '(1 2 3) '(a b c))
;;; ((1 A) (1 B) (1 C) (2 A) (2 B) (2 C) (3 A) (3 B) (3 C))


(defun pythagorean-triples (limit)
  (lc (list x y z)
      (for x 1 limit)
      (for y 1 x)
      (with z^2 (+ (* x x) (* y y)))
      (with z (isqrt z^2))
      (if (= z^2 (* z z)))))

;;; CL-USER> (pythagorean-triples 50)
;;; ((4 3 5) (8 6 10) (12 5 13) (12 9 15) (15 8 17) (16 12 20)
;;;  (20 15 25) (21 20 29) (24 7 25) (24 10 26) (24 18 30)
;;;  (28 21 35) (30 16 34) (32 24 40) (35 12 37) (36 15 39)
;;;  (36 27 45) (40 9 41) (40 30 50) (42 40 58) (44 33 55)
;;;  (45 24 51) (45 28 53) (48 14 50) (48 20 52) (48 36 60))
