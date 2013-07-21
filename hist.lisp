;;;; hist.lisp
;;;; Copyright (c) 2013 Robert Smith

;;;; implements matlab's hist() function: counts the number of
;;;; occurences of each number in a list of numbers

;;;; Using a table
;;;; Linear amortized time and space.

(defun hash-table-list (table)
  (let ((list nil))
    (maphash (lambda (k v) (push (cons k v) list)) table)
    list))

(defun hist-a (numbers)
  (let ((table (make-hash-table)))
    (dolist (number numbers (hash-table-list table))
      (setf (gethash number table)
            (1+ (gethash number table 0))))))


;;;; Shitty functional way
;;;; Quadratic time, linear space

(defun hist-b (numbers)
  (mapcar (lambda (unique-number)
            (cons unique-number
                  (count unique-number numbers)))
          (remove-duplicates numbers)))


;;;; Test

(defun random-numbers (amount max)
  (loop :repeat amount
        :collect (random (1+ max))))

(defun test (&optional (amount 1000))
  (let ((randoms (random-numbers amount most-positive-fixnum)))
    (gc :full t)
    (time (hist-a randoms))
    (gc :full t)
    (time (hist-b randoms))
    nil))

;;                 Table   Functional  
;; (test 20000) => 0.003     6.256
;; (test 40000) => 0.008    25.375
;; (test 80000) => 0.020   110.561
