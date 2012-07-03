;;;; partitions.lisp
;;;; Copyright (c) 2012 Robert Smith

(defun partitions (n &key size (minimum 1))
  "Compute the partitions of N. If SIZE is not provided, compute all
partitions. If SIZE is provided, only compute partitions of that
size. Values of the partition will be greater than or equal to
MINIMUM."
  (let ((result nil))
    (labels ((parts (n size i part)
               (if (= size 1)
                   ;; Base case, return the collected partition.
                   (push (cons n part) result)
                   
                   ;; General case: recurse, reducing the size.
                   (loop :for j :from i :to (floor n size)
                         :do (parts (- n j)
                                    (1- size)
                                    j
                                    (cons j part))))))
      (if (null size)
          ;; Compute all sizes.
          (loop :for k :from 1 :to n
                :do (parts n k minimum nil))
          
          ;; Compute a particular size.
          (parts n size minimum nil))
      
      ;; Return the collected result.
      result)))
