;;;; seqmatch.lisp
;;;; Copyright (c) 2013 Robert Smith

;;;; This file contains algorithms for doing operations related to
;;;; matching sequences.

(defconstant infinity
  #+lispworks +1D++0
  #-lispworks most-positive-double-float)

;;;; Dynamic Time Warping

(defun manhattan-distance (x y)
  (abs (- x y)))

(defun dtw-distance-no-window (a b metric)
  (let* ((n (length a))
         (m (length b))
         (dtw (make-array (list (1+ n) (1+ m)) :initial-element 0)))
    
    (loop :for i :from 1 :to n :do
      (setf (aref dtw i 0) infinity))

    (loop :for i :from 1 :to m :do
      (setf (aref dtw 0 i) infinity))
    
    (loop :for i :from 1 :to n :do
      (loop :for j :from 1 :to m :do
              (setf (aref dtw i j)
                    (+ (funcall metric
                                (aref a (1- i)) 
                                (aref b (1- j)))
                       (min (aref dtw (1- i) j)
                            (aref dtw i      (1- j))
                            (aref dtw (1- i) (1- j)))))))
    
    (aref dtw n m)))

(defun dtw-distance-window (a b window metric)
  (let* ((n (length a))
         (m (length b))
         (w (max window (abs (- n m))))
         (dtw (make-array (list (1+ n) (1+ m)) :initial-element 0)))
    
    (loop :for i :to n :do
      (loop :for j :to m :do
        (setf (aref dtw i j) infinity)))

    (setf (aref dtw 0 0) 0)
    
    (loop :for i :from 1 :to n :do
      (loop :for j :from (max 1 (- i w)) :to (min m (+ i w)) :do
              (setf (aref dtw i j)
                    (+ (funcall metric
                                (aref a (1- i)) 
                                (aref b (1- j)))
                       (min (aref dtw (1- i) j)
                            (aref dtw i      (1- j))
                            (aref dtw (1- i) (1- j)))))))
    
    (aref dtw n m)))

(defun dtw-distance (a b &key window (metric #'manhattan-distance))
  (if window
      (dtw-distance-window a b window metric)
      (dtw-distance-no-window a b metric)))


;;;; TODO: Levenshtein Distance

;;;; Longest Common Subsequence

(defun lcs-matrix (a b &key (test #'eql))
  (let* ((m (length a))
         (n (length b))
         (c (make-array (list (1+ m) (1+ n)) :element-type 'unsigned-byte
                                             :initial-element 0)))
    (loop :for i :from 1 :to n :do
      (loop :for j :from 1 :to m :do
        (setf (aref c i j)
              (if (funcall test (aref a (1- i)) (aref b (1- j)))
                  (1+ (aref c (1- i) (1- j)))
                  (max (aref c (1- i) j)
                       (aref c i (1- j)))))))
    (values c (aref c m n))))

(defun lcs-length (a b &key (test #'eql))
  (multiple-value-bind (c len) (lcs-matrix a b :test test)
    (declare (ignore c))
    len))


