;;;; bit-vectors.lisp
;;;; Copyright (c) 2012 Robert Smith

(defun integer-to-bit-vector (int &optional (len (integer-length int)))
  "Convert a positive integer INT to a BIT-VECTOR."
  (let ((vec (make-array len :element-type 'bit
                             :initial-element 0)))
    (labels ((rec (int bit-posn)
               (cond
                 ((zerop int)       vec)
                 ((= bit-posn len)  vec)
                 (t (multiple-value-bind (quo rem) (floor int 2)
                      (setf (aref vec bit-posn) rem)
                      (rec quo (1+ bit-posn)))))))
      (rec int 0))))

