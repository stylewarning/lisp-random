;;;; m-of-n-circuit.lisp
;;;; Copyright (c) 2011 Robert Smith

(defun any-1-of-3 (a b c)
  (or a b c))

(defun any-2-of-3 (a b c)
  (if a (or b c) (and b c)))

(defun any-3-of-3 (a b c)
  (and a b c))

;;; We could optimalize to check that there are >= M inputs.
(defun any-m-of-n-slow (m inputs)
  "Check that any M of the INPUTS is T, eagerly evaluating the
inputs."
  (cond
    ((zerop m) t)
    ((null inputs) nil)
    ((car inputs) (any-m-of-n (1- m) (cdr inputs)))
    (t (any-m-of-n m (cdr inputs)))))

;;; Here we keep track of the number of inputs so we can fail quickly.
(defun any-m-of-n (m &rest inputs)
    "Check that any M of the INPUTS is T, eagerly evaluating the
inputs. Fail as soon as we reach fewer inputs than M."
  (labels ((any (m len inputs)
             (cond
               ((zerop m) t)
               ((> m len) nil)
               ((car inputs) (any (1- m) (1- len) (cdr inputs)))
               (t (any m (1- len) (cdr inputs))))))
    (any m (length inputs) inputs)))

(defun build-circuit (m inputs)
  "Build a boolean circuit which checks if M of the INPUTS are T."
  (check-type m integer)
  (check-type inputs list)
  
  (labels ((any (m len inputs)
             (cond
               ((and (= 1 m) (< 1 len)) `(or ,@inputs))
               ((= 1 m len)  (car inputs))
               ((= m len)    `(and ,@inputs))
               (t            `(if ,(car inputs)
                                  ,(any (1- m) (1- len) (cdr inputs))
                                  ,(any m (1- len) (cdr inputs)))))))
    (let ((len (length inputs)))
      (cond ((not (plusp m)) t)
            ((> m len) nil)
            (t         (any m len inputs))))))

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

(define-compiler-macro any-m-of-n (&whole form m &rest inputs &environment env)
  ;; First, delete any known null inputs. They decrease the circuit
  ;; size. This optimization might also get done by the compiler after
  ;; a circuit is computed.
  ;; 
  ;; Additionally, we remove any values which are "known truths", and
  ;; record the number of them, so we can decrease M.
  (multiple-value-bind (scrubbed-inputs truths)
      (remove-true-like-forms (remove-if #'null inputs) env)
    (cond ((integerp m) (build-circuit (- m truths) scrubbed-inputs))
          ((plusp truths) `(any-m-of-n (- m ,truths) ,@scrubbed-inputs))
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
;; (ANY-M-OF-N (- M 3) D C D E F G)
