;;;; factorial.lisp
;;;; Copyright (c) 2012 Robert Smith

;;; Sieve of Eratosthenes. Not optimized.
(defun primes (limit)
  "Compute a list of primes less than or equal to LIMIT."
  (if (< limit 2)
      nil
      (let* ((len (+ (floor limit 2)
                     (mod limit 2)
                     -1))
             (sieve (make-array (1+ len)
                                :element-type 'bit
                                :initial-element 1)))
        (loop :for i :below (floor (floor (sqrt limit)) 2)
              :unless (zerop (aref sieve i))
                :do (loop :for j :from (+ 3 (* 2 i (+ 3 i))) :below len
                            :by (+ 3 (* 2 i))
                          :do (setf (aref sieve j) 0))
              :finally (return
                         (cons 2
                               (loop :for i :below len
                                     :unless (zerop (aref sieve i))
                                       :collect (+ 3 (* 2 i)))))))))

(defun count-factor (n factor)
  "Count the number of times a prime FACTOR appears in the prime
factorization of (FACTORIAL N)."
  (labels ((rec (dividend divisor count)
             (if (> factor dividend)
                 count
                 (let ((delta (floor dividend factor)))
                   (rec delta (+ count delta))))))
    (rec n 0)))

;;; We can compute the number of trailing zeroes by counting the
;;; number of times 5 appears in the factorization of (FACTORIAL N).
(defun trailing-zeroes (n)
  "Compute the number of trailing zeroes to (FACTORIAL N)."
  (count-factor n 5))

;;; This is also known as "de Polignac's formula".
(defun factorial-factorization (n)
  "Compute the prime factorization of (FACTORIAL N)."
  ;; Compute the primes up to N.
  (let ((primes (primes n)))
    (loop :for prime :in primes
          :collect (list prime
                         (count-factor n prime)))))

(defun multiply-factorization (factorization)
  "Compute the product of FACTORIZATION."
  (let ((product 1))
    (loop :for (prime exponent) :in factorization
          :do (setf product (* product (expt prime exponent)))
          :finally (return product))))

;;; This could be made faster by caching the primes, and doing some
;;; divide-and-conquer work on the products.

(defun factorial (n)
  "Compute the factorial of N."
  (multiply-factorization (factorial-factorization n)))

(defun factorial-fusion (n)
  "Compute the factorial of N using a fusion of the functions
  FACTORIAL-FACTORIZATION and MULTIPLY-FACTORIZATION."
  ;; This code duplicates the fused functions.
  (let ((primes (primes n))
        (product 1))
    (loop :for prime :in primes
          :do (setf product (* product
                               (expt prime (count-factor n prime))))
          :finally (return product))))