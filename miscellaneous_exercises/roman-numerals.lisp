;;;; roman-numerals.lisp
;;;; Copyright (c) 2012 Robert Smith

;;;; Convert Roman numerals to decimal in O(N).

(defvar *roman-letters* '((#\M . 1000)
                          (#\D .  500)
                          (#\C .  100)
                          (#\L .   50)
                          (#\X .   10)
                          (#\V .    5)
                          (#\I .    1)))


(defun number->roman (n)
  (assert (and (integerp n)
               (plusp n)))
  (format nil "~@R" n))

(defun roman-letter->value (letter)
  (cdr (assoc letter *roman-letters*)))

(defun string->list (string)
  (coerce string 'list))

(defun roman->number (roman)
  (let ((letters (map 'list #'roman-letter->value roman)))
    (labels ((rec (numbers cur acc)
               (if (null numbers)
                   (+ cur acc)
                   (let ((next (car numbers)))
                     (if (> next cur)
                         (rec (cdr numbers)
                              (- next cur)
                              acc)
                         (rec (cdr numbers)
                              next
                              (+ cur acc)))))))
      (rec (cdr letters)
           (car letters)
           0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Test ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun id (n)
  (let* ((nr (number->roman n))
         (rn (roman->number nr)))
    (values rn nr)))

(defun test (&optional (trials 1000))
  (loop :repeat trials
        :for rand := (1+ (random 3999)) :then (1+ (random 3999))
        :unless (= rand (id rand))
          :collect (cons rand (number->roman rand))))
