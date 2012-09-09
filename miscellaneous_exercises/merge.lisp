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
      (dotimes (i (+ len1 len2) final)
        (set-next)))))

(defun merge-k (arrays)
  (let ((cnt (length arrays)))
    (case cnt
      ((0) nil)
      ((1) (first arrays))
      ((2) (merge-2 (first arrays)
                    (second arrays)))
      (otherwise (merge-2 (merge-k (subseq arrays 0 (floor cnt 2)))
                          (merge-k (subseq arrays (floor cnt 2))))))))


;;; a more efficient merge...

(defun <* (a b)
  ;; NIL = Infinity
  (cond
    ((null a) nil)
    ((null b) t)
    (t (< a b))))

(defun merge-k-opt (arrays)
  (if (= 1 (length arrays))
      (first arrays)
      (let* ((cursors (make-array (length arrays) :initial-element 0))
             (lengths (map 'vector #'length arrays))
             (final-length (reduce #'+ lengths))
             (final (make-array final-length)))
        (labels ((peek (i)
                   "Peek at the value at the cursor in array I."
                   (let ((cursor (aref cursors i))
                         (len    (aref lengths i)))
                     (if (= cursor len)
                         nil
                         (aref (nth i arrays) cursor))))
                 
                 (compute-next ()
                   "Find the minimum element at or above each array's
                cursor in ARRAYS."
                   (loop :with min-index   := nil
                         :and  min-element := nil
                         :for i :from 0
                         :for a :in arrays
                         :do (let ((peeked-value (peek i)))
                               (when (<* peeked-value min-element)
                                 (setf min-element peeked-value
                                       min-index   i)))
                         :finally (return (values min-element min-index)))))
          (loop :for i :below final-length
                :do (multiple-value-bind (min-elt min-idx) (compute-next)
                      ;; Save the minimum into the final array.
                      (setf (aref final i) min-elt)
                      
                      ;; Increment the cursor for the array with the minimum.
                      (incf (aref cursors min-idx)))
                :finally (return final))))))