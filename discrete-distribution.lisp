;;;; discrete-distribution.lisp
;;;; Copyright (c) 2013 Robert Smith

;; Not actually used here. Just wrote it because it's cool.
(defmacro sum ((var min max) &body body)
  "Sum the value of BODY with VAR ranging from MIN to MAX inclusive."
  (let ((gmin (gensym "MIN-"))
        (gmax (gensym "MAX-")))
    `(let ((,gmin ,min)
           (,gmax ,max))
       (loop :for ,var :from ,gmin :to ,gmax
             :sum (progn ,@body)))))

;; DIST should be a list of (VALUE . TALLY) pairs. Typically, for a
;; mathematical distribution, TALLEY should represent P(VALUE).

(defun size (dist)
  "Compute the size of DIST."
  (reduce (lambda (x y)
            (+ x (cdr y)))
          data
          :initial-value 0))

(defun mean (data)
  "Compute the mean of DIST."
  (/ (reduce (lambda (mean pt)
               (+ mean (* (car pt) (cdr pt))))
             data
             :initial-value 0)
     (size data)))
