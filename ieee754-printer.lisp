(defconstant +positive-infinity+
  #b01111111100000000000000000000000)

(defconstant +negative-infinity+
  #b11111111100000000000000000000000)

(defconstant +positive-zero+
  #b00000000000000000000000000000000)

(defconstant +negative-zero+
  #b10000000000000000000000000000000)

;;; Just examples, not unique
(defconstant +nan+
  #b01111111100000000000000000000001)

;;; Actual Code

(defun partition-float (bits)
  "Returns three values: the mantissa, exponent, and sign."
  (declare (type (unsigned-byte 32) bits))
  (values (ldb (byte 23 0) bits)        ; Mantissa
          (ldb (byte 8 23) bits)        ; Exponent
          (ldb (byte 1 31) bits)))      ; Sign

(defun nth-bit (n bits)
  "Get the Nth bit of BITS."
  (ldb (byte 1 n) bits))

;;; FIXME: Incorrect
(defun print-mantissa (mantissa stream &key (normalp t))
  (if normalp
      (let ((fraction
              (/ (+ mantissa #x800000)
                 #x800000)))
        (format stream "<NORMAL-MANTISSA: ~a>" fraction))
      (progn
        (format stream "<SUB-NORMAL-MANTISSA>"))))

(defun print-float (bits &optional (stream *standard-output*))
  "Print a 32-bit IEEE-754 float."
  (declare (type (unsigned-byte 32) bits))
  (multiple-value-bind (mantissa exponent sign)
      (partition-float bits)
    (let ((all-ones #b11111111))
      (cond
        ;; Zero
        ((and (zerop mantissa)
              (zerop exponent))
         (princ (if (zerop sign) "+0.0" "-0.0") stream))

        ;; Infinity
        ((and (zerop mantissa)
              (= all-ones exponent))
         (princ (if (zerop sign) "+inf" "-inf") stream))
        
        ;; NaN
        ((= all-ones exponent)
         (princ "NaN" stream))
        
        ;; Denormalized
        ;; FIXME: Incorrect
        ((zerop exponent)
         (unless (zerop sign)
           (princ "-" stream))
         (print-mantissa mantissa stream :normalp nil)
         (format stream "e-" (- 149 (integer-length mantissa))))
        
        ;; Normalized
        ;; FIXME: Incorrect
        (t
         (unless (zerop sign)
           (princ "-" stream))
         (print-mantissa mantissa stream :normalp t)
         (format stream "e" (- exponent 127))))))
  
  ;; Return the argument printed.
  bits)

(defun test ()
  (labels ((print-float/bits (bits)
             (terpri)
             (princ "    Printed: ")
             (print-float bits)
             (terpri)
             (princ "    Bits   : ")
             (format t "~32,'0B~%" bits)))
    
    (princ "Positive Zero: ")
    (print-float/bits +positive-zero+)
    
    (princ "Negative Zero: ")
    (print-float/bits +negative-zero+)
    
    (princ "Positive Infinity: ")
    (print-float/bits +positive-infinity+)
    
    (princ "Negative Infinity: ")
    (print-float/bits +negative-infinity+)
    
    (princ "NaN: ")
    (print-float/bits +nan+)

    (princ "Sub-Normal: ")
    (print-float/bits )
    )
  
  
  )