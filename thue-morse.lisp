;;;; thue-morse.lisp
;;;; Copyright (c) 2013 Robert Smith

(declaim (optimize speed
                   (safety 0)
                   (debug 0)))

(deftype array-bound ()
  `(integer 0 ,array-total-size-limit))

(declaim (inline flip))
(defun flip (n)
  (declare (type bit n))
  (the bit (- 1 n)))

(declaim (inline smallest-power-of-2-greater-than))
(defun smallest-power-of-2-greater-than (n)
  (declare (type array-bound n))
  (expt 2 (ceiling (log n 2))))

(declaim (inline grow-to))
(defun grow-to (array n)
  (declare (type simple-bit-vector array)
           (type array-bound n))
  (replace (make-array n :element-type 'bit
                         :initial-element 0)
           array))

(defun thue-morse-bit (n)
  "Find the Nth bit of the Thue-Morse sequence, starting from zero."
  (declare (type (integer 0) n))
  (cond
    ((zerop n) 0)
    ((evenp n) (thue-morse-bit (floor n 2)))
    (t (flip (thue-morse-bit (floor n 2))))))

(let ((bits #*0))
  (defun thue-morse (n)
    "Compute the Thue-Morse sequence for at least the number of
iterations to compute (THUE-MORSE-BIT N). The result is memoized."
    (declare (type array-bound n))
    (let ((length (length bits)))
      (if (< n length)
          bits
          (let ((to (smallest-power-of-2-greater-than n)))
            (labels ((rec (from)
                       (declare (type array-bound from))
                       (if (= from to)
                           bits
                           (dotimes (i from (rec (* 2 from)))
                             (setf (aref bits (+ i from))
                                   (flip (aref bits i)))))))
              
              (setf bits (grow-to bits to))
              (rec length)))))))
