(defconstant ii #C(0 1))

(defconstant 2*pi*i (* 2 pi ii))

(defun primitive-nth-root (n &optional (k 1))
  (if (= n k)
      1
      (exp (/ (* 2*pi*i k) n))))

(defun slice (v indexes)
  (loop
    :for i :in indexes
    :collect (aref v i) :into s
    :finally (return (coerce s 'vector))))

(defun dft (x &key (normalization-factor (/ (sqrt (length x))))
                   inversep)
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

;;; This has a normalization factor of 1
(defun fft (x)
  (let ((N (length x)))
    (if (= 1 N)
        x
        (let* ((m (floor n 2))
               (top (fft (slice x (qtl:range 0 N 2))))
               (bot (fft (slice x (qtl:range 1 N 2))))
               (d (map 'vector
                       (lambda (k) (primitive-nth-root N (- k)))
                       (qtl:iota m)))
               (z (map 'vector #'* d bot)))
          (concatenate 'vector
                       (map 'vector (lambda (vi zi) (+ vi zi)) top z)
                       (map 'vector (lambda (vi zi) (- vi zi)) top z))))))

