;;;; random-in-stream.lisp
;;;; Copyright (c) 2013 Robert Smith

;;;; Randomly select an element from a stream using only two memory
;;;; cells, with equal probability.

(defun random! (seq)
  "Choose a random element from the sequence SEQ (represented as a
mutable list)."
  (labels ((rec (rand-elt count)
             (if (null seq)
                 rand-elt
                 (let ((next (pop seq)))
                   (rec (if (zerop (random count))
                            next
                            rand-elt)
                        (1+ count))))))
    (rec (pop seq) 2)))


;;; Test

(defun range (n)
  (loop :for i :below n :collect i))

(defun test-rng (trials max)
  (let ((ht (make-hash-table)))
    
    (dotimes (i max)
      (setf (gethash i ht) 0))
    
    (loop :repeat trials
          :do (incf (gethash (random! (range max)) ht)))
    
    (loop :with expected := (float (/ trials max))
          :for i :below max
          :collect (/ (gethash i ht) expected))))
