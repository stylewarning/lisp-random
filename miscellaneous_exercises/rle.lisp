(defun prepend (x xs)
  "Prepend X to XS."
  (cons x xs))

;;; [T] -> [(T . Integer)]
(defun rle-2 (list)
  (if (null list)
      nil
      (reverse
       (reduce (lambda (acc v)
                 (destructuring-bind ((cur . count) . xs) acc
                   (if (eql cur v)
                       (prepend (cons cur (1+ count)) xs)
                       (prepend (cons v 1) acc))))
               (cdr list)
               :initial-value (list (cons (car list) 1))))))
