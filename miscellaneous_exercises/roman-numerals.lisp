;;;; roman-numerals.lisp
;;;; Copyright (c) 2012 Robert Smith

;;;; Convert decimal to Roman numerals in O(N).

#+#:ignore
(defun number->roman (n)
  (assert (and (integerp n)
               (plusp n)))
  (format nil "~@R" n))

(defvar *units* '((1000 . "M")
                  (900  . "CM")
                  (500  . "D")
                  (400  . "CD")
                  (100  . "C")
                  (90   . "XC")
                  (50   . "L")
                  (40   . "XL")
                  (10   . "X")
                  (9    . "IX")
                  (5    . "V")
                  (4    . "IV")
                  (1    . "I")))

(defun roman-floor (number)
  (loop :with max := nil
        :for pair :in *units*
        :when (<= (car pair) number)
          :do (return (values (car pair) (cdr pair)))
        :finally (error "invalid number")))

(defun number->roman (number)
  (labels ((roman-numerals (number letters)
             (if (zerop number)
                 (nreverse letters)
                 (multiple-value-bind (floor letter)
                     (roman-floor number)
                   (roman-numerals (- number floor)
                                   (cons letter letters))))))
    (with-output-to-string (*standard-output*)
      (mapc #'write-string (roman-numerals number nil)))))

;;;; Convert Roman numerals to decimal in O(N).

(defvar *roman-letters* '((#\M . 1000)
                          (#\D .  500)
                          (#\C .  100)
                          (#\L .   50)
                          (#\X .   10)
                          (#\V .    5)
                          (#\I .    1)))


(defun roman-letter->value (letter)
  (cdr (assoc letter *roman-letters*)))

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
