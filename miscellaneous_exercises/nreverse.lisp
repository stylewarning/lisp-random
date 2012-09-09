;;;; nreverse.lisp
;;;; Copyright (c) 2012 Robert Smith

;;;; Reverse a list in constant space and linear time.

(defun nrev (list)
  (if (null list)
      nil
      (let ((remaining (cdr list))
            (reversed  (rplacd list nil))
            (temp      nil))
        (loop :while remaining
              :do (progn
                    (setq temp remaining)
                    (setq remaining (cdr remaining))
                    (rplacd temp reversed)
                    (setq reversed temp))
              :finally (return reversed)))))

;;; For the sake of completeness, here's a linear space, linear time
;;; functional reversal.

(defun rev (list)
  (labels ((reversal (remaining accum)
             (if (null remaining)
                 accum
                 (reversal (cdr remaining)
                           (cons (car remaining) accum)))))
    (reversal list nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Test ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; All of the timings should say "0 bytes consed"

(defun test ()
  (dotimes (i 6)
    (let ((x (iota (expt 10 i))))
      (assert (equalp (reverse x)
                      (time (nrev x)))))))