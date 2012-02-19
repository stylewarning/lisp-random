(declaim (optimize speed))

(defconstant +matrix-count+ (expt 2 20))

(defun 5x5-matrix ()
  (make-array '(5 5) :element-type 'lla:lla-double
                     :initial-element -1.0d0))

(defun integer->rc (int)
  (declare (type (integer 0 20) int))
  
  (case int
    ((0) (values 0 1))
    ((1) (values 0 2))
    ((2) (values 0 3))
    ((3) (values 0 4))
    
    ((4) (values 1 0))
    ((5) (values 1 2))
    ((6) (values 1 3))
    ((7) (values 1 4))
    
    ((8) (values 2 0))
    ((9) (values 2 1))
    ((10) (values 2 3))
    ((11) (values 2 4))
    
    ((12) (values 3 0))
    ((13) (values 3 1))
    ((14) (values 3 2))
    ((15) (values 3 4))
    
    ((16) (values 4 0))
    ((17) (values 4 1))
    ((18) (values 4 2))
    ((19) (values 4 3))))

(defun integer-to-matrix (int)
  (assert (not (minusp int)))
  
  (let ((mat (5x5-matrix))
        (n 0))
    
    (loop :for i :below 5
          :do (setf (aref mat i i) 0.0d0))
    
    (labels ((binary (int)
               (if (zerop int)
                   mat
                   (progn
                     (multiple-value-bind (row col) (integer->rc n)
                       (setf (aref mat row col)
                             (if (evenp int) -1.0d0 1.0d0)))
                     (incf n)
                     (binary (ash int -1))))))
      (binary int))))

(defun almost-zero (number &optional (tolerance 1000))
  (zerop (floor (* number tolerance))))

(defun null-space (m)
  (let* ((svd (lla:svd m))
         (d (lla::diagonal-elements (lla:svd-d svd)))
         (v (lla::transpose (lla:svd-vt svd))))
    (let ((pos (position-if #'almost-zero d :from-end t)))
      (when pos
        (aref (lla::matrix-to-columns v) pos)))))

(defun rationalise (n)
  (/ (floor (* n 10000)) 10000))

(defparameter *kernels* (make-array +matrix-count+))

(defun compute-null-spaces ()
  (loop :for i :below +matrix-count+
        :for m := (integer-to-matrix i)
        :do (setf (aref *kernels* i)
                  (null-space m))))

(defun null-space-indexes ()
  (loop :for i :from 0
        :for m :across *kernels*
        :if m
          :collect i))