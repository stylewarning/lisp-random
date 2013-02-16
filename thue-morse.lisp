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

(defun thue-morse-bit (n)
  (declare (type (integer 0) n))
  (cond
    ((zerop n) 0)
    ((evenp n) (thue-morse-bit (floor n 2)))
    (t (flip (thue-morse-bit (floor n 2))))))

(let ((bits (make-array 1 :element-type 'bit
                          :initial-element 0
                          :adjustable t)))
  (defun thue-morse-displaced (n)
    (declare (type array-bound n))
    (labels ((rec (length)
               (declare (type array-bound length))
               (if (< n length)
                   bits
                   (let ((double-length (* 2 length)))
                     (adjust-array bits double-length :element-type 'bit
                                                      :initial-element 0)
                     (let ((upper-half (make-array length :element-type 'bit
                                                          :displaced-to bits
                                                          :displaced-index-offset length)))
                       (map-into upper-half #'flip bits)
                       (rec double-length))))))
      (rec (length bits)))))

(let ((bits (make-array 1 :element-type 'bit
                          :initial-element 0
                          :adjustable t)))
  (defun thue-morse-bit-not (n)
    (declare (type array-bound n))
    (labels ((rec (length)
               (declare (type array-bound length))
               (if (< n length)
                   bits
                   (progn
                     (setf bits
                           (concatenate 'bit-vector bits (bit-not bits)))
                     (rec (* 2 length))))))
      (rec (length bits)))))

(let ((bits (make-array 1 :element-type 'bit
                          :initial-element 0
                          :adjustable t)))
  (defun thue-morse-loop (n)
    (declare (type array-bound n))
    (labels ((rec (length)
               (declare (type array-bound length))
               (if (< n length)
                   bits
                   (let ((double-length (* 2 length)))
                     (adjust-array bits double-length :element-type 'bit
                                                      :initial-element 0)
                     (dotimes (i length)
                       (setf (aref bits (+ i length))
                             (flip (aref bits i))))
                     (rec double-length)))))
      (rec (length bits)))))

(defun test (n)
  (gc :full t)
  (time (thue-morse-displaced n))
  (gc :full t)
  (time (thue-morse-bit-not n))
  (gc :full t)
  (time (thue-morse-loop n))
  (gc :full t)
  t)
