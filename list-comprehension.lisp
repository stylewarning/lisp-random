;;;; list-comprehension.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

;;;; A quick and dirty implementation of list comprehensions. Could be
;;;; used as a basis for extensible list comprehensions, e.g.,
;;;;
;;;;    (define-lc-clause ...)

(defmacro with-collector ((var collector) &body body)
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

(defun kw (symb)
  (intern (symbol-name symb) :keyword))

(defmacro lc (expr &rest clauses)
  (let ((result (gensym "RESULT-"))
        (collect (gensym "COLLECT-")))
    (labels ((process-clauses (clauses)
               (if (null clauses)
                   `(,collect ,expr)
                   (destructuring-bind (type &rest clause-items)
                       (car clauses)
                     (case (kw type)
                       ((:for)
                        (process-for (car clause-items) ; variable
                                     (cdr clause-items) ; bounds, etc.
                                     (cdr clauses)))
                       ((:if)
                        (process-if clause-items ; conjunction
                                    (cdr clauses)))
                       ((:with)
                        (process-with (car clause-items) ; variable
                                      (cdr clause-items) ; value
                                      (cdr clauses)))))))
             (process-for (var items clauses)
               (ecase (length items)
                 ((1)
                  ;; (FOR <var> <seq>)
                  `(map nil
                        (lambda (,var)
                          ,(process-clauses clauses))
                        ,(first items)))
                 ((2)
                  ;; (FOR <var> <min> <max>)
                  `(loop :for ,var :from ,(first items) :below ,(second items)
                         :do (progn ,(process-clauses clauses))))))
             (process-if (items clauses)
               ;; (IF <predicate form>*)
               `(when ,(list* 'and items)
                  ,(process-clauses clauses)))
             (process-with (var items clauses)
               ;; FIXME: could do some error checking on ITEMS
               ;; (WITH <var> <value>)
               `(let ((,var ,(car items)))
                  ,(process-clauses clauses))))
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
