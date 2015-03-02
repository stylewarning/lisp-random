;;;; eulerian-numbers.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

(defun replicate (n x) (make-list n :initial-element x))

(defun flip (f) (lambda (y x) (funcall f x y)))

(defun eulerian-diagonal (n)
  "Compute the Nth element along the first non-trivial diagonal of the Euler triangle. Sloane's A000295[n+2]"
  (check-type n (integer 1))
  (* 1/2 (count #\\ (reduce (flip #'funcall)
                            (replicate (+ 3 n) 'prin1-to-string)))))

;;; > (loop :for i :from 1 :to 10 :collect (eulerian-diagonal i))
;;; (1 4 11 26 57 120 247 502 1013 2036)
