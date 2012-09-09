;;;; merge.lisp
;;;; Copyright (c) 2012 Robert Smith

;;;; Given N arrays with at most M elements in sorted order, merge
;;;; them.

(defun merge-2 (array1 array2)
  (let* ((len1 (length array1))
         (len2 (length array2))
         (final (make-array (+ len1 len2)))
         (cursor1 0)
         (cursor2 0))
    (labels ((cursor ()
               (+ cursor1 cursor2))
             
             (peek1 ()
               (if (= cursor1 len1)
                   nil
                   (aref array1 cursor1)))
             
             (peek2 ()
               (if (= cursor2 len2)
                   nil
                   (aref array2 cursor2)))
             
             (min* (a b)
               (cond
                 ((null a) (values b 1))
                 ((null b) (values a 0))
                 (t (if (< a b)
                        (values a 0)
                        (values b 1)))))
             
             (set-next ()
               (let ((p1 (peek1))
                     (p2 (peek2)))
                 (multiple-value-bind (m p) (min* p1 p2)
                   (setf (aref final (cursor)) m) ; Set next element in result
                   (if (zerop p)
                       (incf cursor1)
                       (incf cursor2))))))
      (loop :for i :below (+ len1 len2)
            :do (set-next)
            :finally (return final)))))

(defun merge-k (arrays)
  (let ((cnt (length arrays)))
    (case cnt
      ((0) nil)
      ((1) (first arrays))
      ((2) (merge-2 (first arrays)
                    (second arrays)))
      (otherwise (merge-2 (merge-k (subseq arrays 0 (floor cnt 2)))
                          (merge-k (subseq arrays (floor cnt 2))))))))