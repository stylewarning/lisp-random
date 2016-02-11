;;;; fractran.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

(defun fractran-instruction-p (instruction)
  (and (rationalp instruction)
       (plusp instruction)))

(defun make-fractran-program (&rest instructions)
  (assert (every #'fractran-instruction-p instructions))
  (make-array (length instructions) :initial-contents instructions))

(defun fractran (p n &key (function 'print))
  "Execute the FRACTRAN program P with the initial argument N.

FUNCTION will be applied to each integer value found during the procedure. By default, the values will be printed."
  (check-type n (integer 1) "The initial argument N must be a positive integer.")
  (labels ((iteration (n)
             (loop :for x :across p
                   :for xn := (* x n)
                   :when (integerp xn)
                     :do (return xn)
                   :finally (return nil)))
           (run (n)
             (declare (optimize speed (safety 0) (debug 0)))
             (let ((next (iteration n)))
               (if (null next)
                   n
                   (progn
                     (funcall function next)
                     (run next))))))
    (declare (dynamic-extent #'iteration #'run))
    (run n)))

(defun count-divisors (n d)
  "Count the number of times D evenly divides N."
  (labels ((rec (q c)
             (if (zerop q)
                 (values c q)
                 (multiple-value-bind (quo rem) (floor q d)
                   (if (zerop rem)
                       (rec quo (1+ c))
                       (values c q))))))
    (rec n 0)))

(defvar +add+ (make-fractran-program 3/2)
  "Given an intial number 2^a 3^b, produce a number 3^(a+b).")

(defun add (a b)
  "Add the positive integers A and B."
  (let ((r (fractran +add+ (* (expt 2 a) (expt 3 b)) :function 'identity)))
    (values (count-divisors r 3))))

(defvar +mul+ (make-fractran-program 455/33 11/13 1/11 3/7 11/2 1/3)
  "Given an initial number 2^a 3^b, produce a number 5^(ab).")

(defun mul (a b)
  "Multiply the positive integers A and B."
  (let ((r (fractran +mul+ (* (expt 2 a) (expt 3 b)) :function 'identity)))
    (values (count-divisors r 5))))

(defvar +divrem+ (make-fractran-program 91/66 11/13 1/33 85/11 57/119 17/19 11/17 1/3)
  "Given an initial number 2^n 3^d 11, produce a number 5^q 7^r such that n = q*d + r where 0 <= r < d.")

(defun divrem (a b)
  "Compute as two values the quotient and remainder of A/B."
  (let ((r (fractran +divrem+ (* (expt 2 a) (expt 3 b) 11) :function 'identity)))
    (multiple-value-bind (fives rest)
        (count-divisors r 5)
      (values fives (count-divisors rest 7)))))

(defvar +primes+ (make-fractran-program 17/91 78/85 19/51 23/38 29/33 77/29 95/23 77/19 1/17 11/13 13/11 15/14 15/2 55/1)
  "Given the initial number 2, produce a sequence numbers, which has powers of 2 embedded inside of it. Each successive power of two is a successive-prime-power of two.")

(defun map-primes (f)
  "Map the unary function F across all primes."
  ;; Do the initial prime 2.
  (funcall f 2)
  (labels ((power-of-two (n)
             (zerop (logand n (1- n))))
           (extract-prime (n)
             (1- (integer-length n)))
           (frob (n)
             (when (power-of-two n)
               (funcall f (extract-prime n)))))
    ;; Does not terminate.
    (fractran +primes+ 2 :function #'frob)))

;; TODO: Compile FRACTRAN.
