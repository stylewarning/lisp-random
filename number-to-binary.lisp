;;;; number-to-binary.lisp
;;;; Copyright (c) 2013 Robert Smith

(defun split (x)
  "Split a number X into its whole and fractional parts."
  (let ((floor-x (floor x)))
    (values floor-x
            (- x floor-x))))

(defun to-binary (n max-prec)
  "Convert a rational number N to a list of two sublists: the first a
list of binary digits to the left of the point (in traditional order)
and the second a list of digits to the right of the point."
  (assert (not (minusp n)))
  (labels ((int-binary (n digits)
             (if (zerop n)
                 digits
                 (multiple-value-bind (quo rem)
                     (floor n 2)
                   (int-binary quo (cons rem digits)))))
           (frac-binary (n digits prec)
             (if (or (zerop n)
                     (zerop prec))
                 (nreverse digits)
                 (multiple-value-bind (int frac)
                     (split (* 2 n))
                   (frac-binary frac (cons int digits) (1- prec))))))
    (multiple-value-bind (int frac) (split n)
      (list (if (zerop int)
                nil
                (int-binary int nil))
            (frac-binary frac nil max-prec)))))
