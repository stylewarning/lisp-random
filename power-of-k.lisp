(defun binary-of (n)
  "Compute the binary digits of N compactly."
  (let* ((binary (make-array (integer-length n) :element-type 'bit
                                                :initial-element 0)))
    (labels ((doit (n i)
               (if (zerop n)
                   binary
                   (multiple-value-bind (quo rem)
                       (floor n 2)
                     (setf (bit binary i) rem)
                     (doit quo (1+ i))))))
      (if (zerop n)
          #*0
          (doit n 0)))))

(defun horner (base coeffs)
  "Evaluate the polynomial represented by the array of coefficients
COEFFS at BASE."
  (let ((power 1)
        (sum   0))
    (map nil (lambda (coeff)
               (unless (zerop coeff)
                 (incf sum (* coeff power)))
               (setf power (* power base)))
         coeffs)
    sum))

(defun smallest-sum-of-powers (base k)
  "Find the Kth smallest sum of unique powers of BASE."
  (horner base (binary-of k)))
