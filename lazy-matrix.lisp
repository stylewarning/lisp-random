;;;; lazy-matrix.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

(defclass lazy-matrix ()
  ((height :initarg :height
           :reader height
           :documentation "The height (number of rows) of the matrix.")
   (width :initarg :width
          :reader width
          :documentation "The width (number of columns) of the matrix.")
   (element-ref :initarg :element-ref
                :reader element-ref
                :documentation "A function whose arity is the length of the dimensions of the matrix, and whose value corresponds to a particular element of a matrix."))
  (:documentation "A representation of a lazy matrix."))

(defmethod print-object ((m lazy-matrix) stream)
  (print-unreadable-object (m stream :type t :identity t)
    (format stream "~D x ~D" (height m) (width m))))

(defun mref (m r c)
  "Get the element from the row R and column C of the matrix M."
  (funcall (element-ref m) r c))

(defmacro bind-lazy-matrix ((rows cols ref) matrix &body body)
  (check-type rows symbol)
  (check-type cols symbol)
  (check-type ref symbol)
  `(with-accessors ((,rows height)
                    (,cols width)
                    (,ref element-ref))
       ,matrix
     ,@body))

(defun vector-matrix (cl-vector)
  "Convert a Common Lisp vector CL-VECTOR into a lazy row vector."
  (assert (vectorp cl-vector) (cl-vector))
  (make-instance 'lazy-matrix
                 :height 1
                 :width (length cl-vector)
                 :element-ref (lambda (row col)
                                (assert (zerop row))
                                (aref cl-vector col))))

(defun array-matrix (cl-array)
  "Convert a Common Lisp array CL-ARRAY into a lazy matrix."
  (assert (arrayp cl-array) (cl-array))
  (assert (= 2 (array-rank cl-array)) (cl-array)
          "The array provided must be two dimensional.")
  (destructuring-bind (height width)
      (array-dimensions cl-array)
    (make-instance 'lazy-matrix
                   :height height
                   :width width
                   :element-ref (lambda (row col)
                                  (aref cl-array row col)))))

(defun matrix-array (m &key (element-type t))
  "Convert a lazy matrix M to a Common Lisp array."
  (bind-lazy-matrix (rows cols ref) m
    (let ((array (make-array (list rows cols) :element-type element-type)))
      (dotimes (i rows array)
        (dotimes (j cols)
          (setf (aref array i j) (funcall ref i j)))))))

(defun diagonal-matrix (height width &rest elements)
  "Construct a rectangular matrix of dimensions HEIGHT x WIDTH whose diagonal are ELEMENTS."
  (check-type height (integer 1))
  (check-type width (integer 1))
  (let ((diagonal (coerce elements 'vector)))
    (make-instance 'lazy-matrix
                   :height height
                   :width width
                   :element-ref (lambda (row col)
                                  (if (= row col)
                                      (aref diagonal row)
                                      0)))))

(defun identity-matrix (length)
  "Construct an identity matrix of dimensions LENGTH x LENGTH."
  (make-instance 'lazy-matrix
                 :width length
                 :height length
                 :element-ref (lambda (row col)
                                (if (= row col)
                                    1
                                    0))))

(defun diag (m)
  "Compute the diagonal of the matrix M as a row vector."
  (bind-lazy-matrix (rows cols ref) m
    (make-instance 'lazy-matrix
                   :width (min rows cols)
                   :height 1
                   :element-ref (lambda (row col)
                                  (assert (zerop row) (row))
                                  (funcall ref col col)))))

(defun cache-matrix (m)
  "Create a new matrix which caches the elements of the lazy matrix M upon access."
  (bind-lazy-matrix (rows cols ref) m
    (let* ((g (gensym "UNCACHED-ITEM-"))
           (cache (make-array (* rows cols) :initial-element g)))
      (make-instance 'lazy-matrix
                     :width cols
                     :height rows
                     :element-ref (lambda (row col)
                                    (let* ((index (+ col (* row rows)))
                                           (el (aref cache index)))
                                      (if (eq el g)
                                          (setf (aref cache index)
                                                (funcall ref row col))
                                          el)))))))

(defun matrix-vectorp (m)
  "Is the lazy matrix M a vector? Return T or NIL as the first value, and {:UNIT, :ROW, :COLUMN} as a second value, depending on whether M is a 1x1 matrix, row-vector, or column-vector respectively."
  (let ((unit-height? (= 1 (height m)))
        (unit-width? (= 1 (width m))))
    (cond
      (unit-height?
       (if unit-width?
           (values t :unit)
           (values t :row)))

      (unit-width?
       (values t :column))
      
      (t (values nil nil)))))

(defun matrix-row (m row)
  "Return a row-vector corresponding to the row ROW of the matrix M."
  (bind-lazy-matrix (rows cols ref) m
    (assert (< row rows)
            (row)
            "The row ~D is out of bounds for the matrix ~A."
            row
            m)
    (make-instance 'lazy-matrix
                   :height 1
                   :width cols
                   :element-ref (lambda (r c)
                                  (assert (zerop r))
                                  (funcall ref row c)))))

(defun matrix-column (m col)
  "Return a column-vector corresponding to the column COL of the matrix M."
  (bind-lazy-matrix (rows cols ref) m
    (assert (< col cols)
            (col)
            "The column ~D is out of bounds for the matrix ~A."
            col
            m)
    (make-instance 'lazy-matrix
                   :height rows
                   :width 1
                   :element-ref (lambda (r c)
                                  (assert (zerop c))
                                  (funcall ref r col)))))

(defun transpose (m)
  "Return the transpose of the matrix M."
  (bind-lazy-matrix (rows cols ref) m
    (make-instance 'lazy-matrix
                   :height cols
                   :width rows
                   :element-ref (lambda (r c)
                                  (funcall ref c r)))))

(defun stack (m1 m2)
  "Stack the matrix M1 atop M2. Both M1 and M2 must have the same width."
  (bind-lazy-matrix (rows1 cols1 ref1) m1
    (bind-lazy-matrix (rows2 cols2 ref2) m2
      (assert (= cols1 cols2) (m1 m2) "The matrices ~A and ~A must have the same width." m1 m2)
      (make-instance 'lazy-matrix
                     :height (+ rows1 rows2)
                     :width cols1
                     :element-ref (lambda (r c)
                                    (if (< r rows1)
                                        (funcall ref1 r c)
                                        (funcall ref2 (- r rows1) c)))))))

(defun join (m1 m2)
  "Join the matrices M1 to M2, where M1 is to the left of M2. Both M1 and M2 must have the same height."
  (bind-lazy-matrix (rows1 cols1 ref1) m1
    (bind-lazy-matrix (rows2 cols2 ref2) m2
      (assert (= rows1 rows2) (m1 m2) "The matrices ~A and ~A must have the same height." m1 m2)
      (make-instance 'lazy-matrix
                     :height rows1
                     :width (+ cols1 cols2)
                     :element-ref (lambda (r c)
                                    (if (< c cols1)
                                        (funcall ref1 r c)
                                        (funcall ref2 r (- c cols1))))))))

(defun submatrix (m row1 col1 row2 col2)
  "Extract the submatrix of the matrix M starting at (ROW1, COL1) and ending at (ROW2, COL2). This is sometimes called a block."
  (assert (<= row1 row2))
  (assert (<= col1 col2))
  (bind-lazy-matrix (rows cols ref) m
    (make-instance 'lazy-matrix
                   :height (- col2 col1)
                   :width (- row2 row1)
                   :element-ref (lambda (r c)
                                  (funcall ref
                                           (+ r row1)
                                           (+ c col1))))))

(macrolet ((define-pointwise-operator (name primitive-op doc)
             `(defun ,name (m1 m2)
                ,doc
                (bind-lazy-matrix (rows1 cols1 ref1) m1
                  (bind-lazy-matrix (rows2 cols2 ref2) m2
                    (assert (= rows1 rows2))
                    (assert (= cols1 cols2))
                    (make-instance 'lazy-matrix
                                   :height rows1
                                   :width cols1
                                   :element-ref (lambda (r c)
                                                  (,primitive-op
                                                   (funcall ref1 r c)
                                                   (funcall ref2 r c)))))))))
  (define-pointwise-operator .+ +
    "Compute the pointwise sum of two matrices.")
  (define-pointwise-operator .- -
    "Compute the pointwise difference of two matrices.")
  (define-pointwise-operator .* *
    "Compute the pointwise product of two matrices.")
  (define-pointwise-operator ./ /
    "Compute the pointwise quotient of two matrices."))

(defun matrix-multiply (m1 m2)
  "Multiply the matrices M1 and M2."
  (bind-lazy-matrix (rows1 cols1 ref1) m1
    (bind-lazy-matrix (rows2 cols2 ref2) m2
      (assert (= cols1 rows2) (m1 m2)
              "The number of columns of the first matrix must ~
               be equal to the number of rows of the second ~
               matrix.")
      (make-instance 'lazy-matrix
                     :height rows1
                     :width cols2
                     :element-ref (lambda (r c)
                                    (loop :for k :below cols1
                                          :sum (* (funcall ref1 r k)
                                                  (funcall ref2 k c))))))))

(defun constant-matrix (height width value)
  "Make a lazy matrix whose entries are VALUE of height HEIGHT and width WIDTH."
  (make-instance 'lazy-matrix
                 :width width
                 :height height
                 :element-ref (lambda (r c)
                                (declare (ignore r c))
                                value)))

;;; Example

(defun fft (vec)
  "Compute the FFT of the row-vector VEC."
  (labels ((unit (x)
             (constant-matrix 1 1 x))
           (roots (n)
             (make-instance 'lazy-matrix
                            :width n
                            :height 1
                            :element-ref (lambda (r c)
                                           (declare (ignore r))
                                           (if (= c (* 2 n))
                                               1
                                               (exp (/ (* -2 pi #C(0 1) c)
                                                       (* 2 n)))))))
           (evens (v)
             (make-instance 'lazy-matrix
                            :width (/ (width v) 2)
                            :height 1
                            :element-ref (lambda (r c)
                                           (declare (ignore r))
                                           (mref v 0 (* 2 c)))))
           (odds (v)
             (make-instance 'lazy-matrix
                            :width (/ (width v) 2)
                            :height 1
                            :element-ref (lambda (r c)
                                           (declare (ignore r))
                                           (mref v 0 (1+ (* 2 c))))))
           (fft-rec (v)
             (let ((N (width v)))
               (if (= 2 N)
                   (join (unit (+ (mref v 0 0)
                                  (mref v 0 1)))
                         (unit (- (mref v 0 0)
                                  (mref v 0 1))))
                   (let* ((evens (fft-rec (evens v)))
                          (odds  (fft-rec (odds v)))
                          (w*odds (.* odds (roots (/ N 2)))))
                     (join (.+ evens w*odds)
                           (.- evens w*odds)))))))
    (fft-rec vec)))
