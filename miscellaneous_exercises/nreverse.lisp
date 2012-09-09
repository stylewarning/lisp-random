;;;; nreverse.lisp
;;;; Copyright (c) 2012 Robert Smith

;;;; Reverse a list in constant space.

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Test ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; All of the timings should say "0 bytes consed"

(defun test ()
  (dotimes (i 5)
    (let ((x (iota (* 1000 i))))
      (time (nrev x)))))