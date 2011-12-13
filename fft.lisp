(defconstant ii #C(0 1))

(defconstant 2*pi*i (* 2 pi ii))

(defun primitive-nth-root (n &optional (k 1))
  (if (= n k)
      1
      (exp (/ (* 2*pi*i k) n))))

(defun dft (x &key
                (normalization-factor (/ (sqrt (length x))))
                inversep
                )
  (let ((N (length x)))
    (labels ((coeff (k)
               (loop
                 :with inverter := (if inversep 1 -1)
                 :for j :below N
                 :sum (* (aref x j)
                         (primitive-nth-root N (* j k inverter))))))
      (loop :for k :below N
            :collect (* (coeff k) normalization-factor) :into dft-array
            :finally (return (coerce dft-array 'vector))))))

