;;;; m-of-n-circuit.lisp
;;;; Copyright (c) 2011 Robert Smith

(defun any-1-of-3 (a b c)
  (or a b c))

(defun any-2-of-3 (a b c)
  (if a (or b c) (and b c)))

(defun any-3-of-3 (a b c)
  (and a b c))

;;; We could optimalize to check that there are >= M inputs.
(defun any-m-of-n-slow (m &rest x)
  "Check that any M of the inputs X are T, eagerly evaluating the
inputs."
  (check-type m integer)
  
  (labels ((f (m x)
             (cond
               ((zerop m) t)
               ((null x)  nil)
               ((first x) (f (1- m) (rest x)))
               (t         (f m (rest x))))))
    (f m x)))

;;; Here we keep track of the number of inputs so we can fail quickly.
(defun any-m-of-n (m &rest x)
    "Check that any M of the inputs X are T, eagerly evaluating the
inputs. Fail as soon as we reach fewer inputs than M."
  (check-type m integer)

  ;; Here, N keeps track of the length of X.
  (labels ((f (m n x)
             (cond
               ((not (plusp m)) t)
               ((> m n)         nil)
               ((first x)       (f (1- m) (1- n) (rest x)))
               (t               (f m (1- n) (rest x))))))
    (f m (length x) x)))

(defun build-naive-circuit (m x)
  "Build a boolean circuit which checks if M of the inputs X are T."
  (check-type m integer)
  (check-type x list)
  
  (labels ((f (m n x)
             (cond
               ((not (plusp m)) t)
               ((> m n)         nil)
               (t               `(if ,(first x)
                                     ,(f (1- m) (1- n) (rest x))
                                     ,(f m (1- n) (rest x)))))))
    (f m (length x) x)))

(defun build-circuit (m x)
  "Build a boolean circuit which checks if M of the inputs X are T."
  (check-type m integer)
  (check-type x list)
  
  (labels ((f (m n x)
             (cond
               ((and (= 1 m) (< 1 n)) `(or ,@x))
               ((= 1 m n)             (first x))
               ((= m n)               `(and ,@x))
               (t                     `(if ,(first x)
                                           ,(f (1- m) (1- n) (rest x))
                                           ,(f m (1- n) (rest x)))))))
    (let ((n (length x)))
      (cond ((not (plusp m)) t)
            ((> m n)         nil)
            (t               (f m n x))))))

;; CL-USER> (build-circuit 0 '(a b c d e))
;; T
;;
;; CL-USER> (build-circuit 1 '(a b c d e))
;; (OR A B C D E)
;;
;; CL-USER> (build-circuit 2 '(a b c d e))
;; (IF A
;;     (OR B C D E)
;;     (IF B
;;         (OR C D E)
;;         (IF C
;;             (OR D E)
;;             (AND D E))))
;;
;; CL-USER> (build-circuit 3 '(a b c d e))
;; (IF A
;;     (IF B
;;         (OR C D E)
;;         (IF C
;;             (OR D E)
;;             (AND D E)))
;;     (IF B
;;         (IF C
;;             (OR D E)
;;             (AND D E))
;;         (AND C D E)))
;;
;; CL-USER> (build-circuit 4 '(a b c d e))
;; (IF A
;;     (IF B
;;         (IF C
;;             (OR D E)
;;             (AND D E))
;;         (AND C D E))
;;     (AND B C D E))
;;
;; CL-USER> (build-circuit 5 '(a b c d e))
;; (AND A B C D E)
;;
;; CL-USER> (build-circuit 6 '(a b c d e))
;; NIL

(defun true-like (thing &optional environment)
  (and (constantp thing environment)
       (not (null thing))))

(defun remove-true-like-forms (things &optional environment)
  (values (remove-if (lambda (x) (true-like x environment)) things)
          (count-if (lambda (x) (true-like x environment)) things)))

(define-compiler-macro any-m-of-n (&whole form m &rest x &environment env)
  ;; First, delete any known null inputs. They decrease the circuit
  ;; size. This optimization might also get done by the compiler after
  ;; a circuit is computed.
  ;; 
  ;; Additionally, we remove any values which are "known truths", and
  ;; record the number of them, so we can decrease M.
  (multiple-value-bind (scrubbed-x truths)
      (remove-true-like-forms (remove-if #'null x) env)
    (cond ((integerp m) (build-circuit (- m truths) scrubbed-x))
          ((plusp truths) `(any-m-of-n (- ,m ,truths) ,@scrubbed-x))
          (t form))))

;; CL-USER> (funcall (compiler-macro-function 'any-m-of-n)
;;                   '(any-m-of-n 4 'a 'b 'c d nil c d e f g)
;;                   nil)
;; (OR D C D E F G)
;;
;; CL-USER> (funcall (compiler-macro-function 'any-m-of-n)
;;                   '(any-m-of-n 5 'a 'b 'c d nil c d e f g)
;;                   nil)
;; (IF D
;;     (OR C D E F G)
;;     (IF C
;;         (OR D E F G)
;;         (IF D
;;             (OR E F G)
;;             (IF E
;;                 (OR F G)
;;                 (AND F G)))))
;;
;; CL-USER> (funcall (compiler-macro-function 'any-m-of-n)
;;                   '(any-m-of-n n 'a 'b 'c d nil c d e f g)
;;                   nil)
;; (ANY-M-OF-N (- N 3) D C D E F G)
