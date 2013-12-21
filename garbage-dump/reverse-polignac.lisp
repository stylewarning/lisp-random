;;;; reverse-polignac.lisp
;;;; Copyright (c) 2013 Robert Smith

;;; Let phi_p(n) be the largest k such that p^k | n. It is naively
;;; implemented as so:

(defun dividesp (k n)
  "Does K divide N?"
  (zerop (mod n k)))

(defun phi-naive (p n)
  (loop :for p^k := p :then (* p p^k)
        :for k :from 1
        :while (dividesp p^k n)
        :finally (return (1- k))))

;;; Define sigma_p(n) to be the sum of phi_p(i) for i from 1 to n. It
;;; is also naively implemented as so:

(defun sigma-naive (p n)
  (loop :for i :from 1 :to n
        :sum (phi-naive p n)))

;;; The problem is to find the minimum k such that k >= sigma_p(n) for
;;; some given p and n.
