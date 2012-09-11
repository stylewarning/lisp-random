;;;; armstrong.lisp
;;;; Copyright (c) 2012 Robert Smith

;;;; An Armstrong number is a number equal to the sum of the cube of
;;;; its digits.

(defun cube (x)
  "Return the cube of X."
  (* x x x))

(defun digits (n)
  "Return a list of the decimal digits of the integer N."
  (check-type n (integer 0))
  (labels ((rec (n digits)
             (if (zerop n)
                 (nreverse digits)
                 (multiple-value-bind (quo rem) (floor n 10)
                   (rec quo (cons rem digits))))))
    (if (zerop n)
        (list 0)
        (rec n nil))))

(defun armstrong-number-p (n)
  "Check if the number N is an Armstrong number."
  (check-type n (integer 0))
  (= n (reduce #'+ (digits n) :key #'cube)))

(defun armstrongs-below (n)
  "Tabulate the Armstrong numbers below N."
  (loop :for i :below n
        :when (armstrong-number-p i)
          :collect i))