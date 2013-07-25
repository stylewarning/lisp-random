;;;; poly-in-n.lisp
;;;; Copyright (c) 2013 Robert Smith

;;;; Given a blackbox function whose underlying implementation is an
;;;; arbitrary degree polynomial with non-negative integer
;;;; coefficients, recover the polynomial in a minimum number of
;;;; evaluations.

(defun eval-poly (x poly)
  (reduce (lambda (coef sum)
            (+ coef (* x sum)))
          poly
          :from-end t
          :initial-value 0))

;; Given a polynomial f with non-negative integer coefficients c_0,
;; ..., c_d, then for all i, f(1) >= c_i, and f(1) + 1 > c_i. If we
;; compute f(1 + f(1)), then the result can be seen as an integer in
;; base 1 + f(1), so we do reverse Horner to recover the coefficients.

(defun recover-poly (poly)
  (let* ((f1 (eval-poly 1 poly))        ; Evaluation #1
         (fs (eval-poly (1+ f1) poly))) ; Evaluation #2
    ;; v v                                                v v
    ;; v v From here on, we cannot use POLY or EVAL-POLY. v v
    ;; v v                                                v v
    (loop :with total := fs
          :until (zerop total)
          :collect (multiple-value-bind (quo rem) (floor total (1+ f1))
                     (setf total quo)
                     rem))))

;; Examples:
;;
;; CL-USER> (recover-poly '(1 0 1 2))
;; (1 0 1 2)
;; CL-USER> (recover-poly '(8 34295 0 0 0 0 0 0 0 0 1))
;; (8 34295 0 0 0 0 0 0 0 0 1)
