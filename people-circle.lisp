;;;; people-circle.lisp
;;;; Copyright (c) 2013 Robert Smith

;;; Imagine you have a circle of people and you go around the circle
;;; removing every second person until one person is left.
;;;
;;; If you have 3 people in the circle, then the 3rd person will be
;;; the last one remaining.
;;;
;;; If you have 4 people then the 1st person will be the last one
;;; remaining.
;;;
;;; If you have 11 people then the 7th person will be the one
;;; remaining.
;;;
;;; If you have N people in the circle, who will be the last one
;;; remaining?

(setf *print-circle* t)

(defun circle (n)
  (loop :for i :from 1 :to n :collect i :into people
        :finally (return (nconc people people))))

(defun previous (circle)
  (loop :for person :on (cdr circle) :by #'cdr
        :until (eq circle (cdr person))
        :finally (return person)))

(defun remove-nth (circle n)
  (labels ((rec (current last n)
             (if (> n 1)
                 (rec (cdr current) current (- n 1))
                 (progn
                   (rplacd last (cdr current))
                   (cdr current)))))
    (let ((prev (previous circle)))
      (if (eq prev circle)
          nil
          (rec circle prev n)))))

(defun solve (circle &optional (n 2))
  (if (eq circle (cdr circle))
      (car circle)
      (solve (remove-nth circle n) n)))

(defun fastsolve (n)
  (1+ (ash (- n (ash 1 (1- (integer-length n)))) 1)))
