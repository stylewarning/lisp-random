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

(defun catvec (v1 v2)
  (concatenate 'vector v1 v2))

(defun .+ (v1 v2)
  (map 'vector #'+ v1 v2))

(defun .- (v1 v2)
  (map 'vector #'+ v1 v2))

(defun .* (v1 v2)
  (map 'vector #'* v1 v2))

(defun .^ (v1 v2)
  (map 'vector #'expt v1 v2))


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
        (let* ((top   (fft (slice x (qtl:range 0 N 2))))
               (bot   (fft (slice x (qtl:range 1 N 2))))
               (roots (qtl:tabulate (lambda (k) (primitive-nth-root N (- k)))
                                    (floor N 2)))
               (z     (.* roots bot)))
          (catvec (.+ top z) (.- top z))))))

