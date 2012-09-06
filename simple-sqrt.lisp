;;;; simple-sqrt.lisp
;;;; Copyright (c) 2012 Robert Smith

;;;; Here we implement a simple, unoptimized floating-point square
;;;; root.

(defun square-root-estimate (x)
  "Get an estimate for the square root of X."
  1.0d0
  #+#:ignore ; we could do something fancier here
  (cond
    ((plusp x) (let ((len (integer-length (floor x))))))
    )
  )


(defparameter *maximum-iterations* 100)

(defun almost-equal (x y epsilon)
  (or (= x y)
      (< (abs (/ (- x y)
                 (if (> (abs x) (abs y)) x y)))
         epsilon)))

(defun square-root (s &optional (epsilon 0.00001))
  (let ((estimate (square-root-estimate s))
        (iteration-count 0))
    (labels ((iteration (x)
               (/ (+ x (/ s x)) 2))
             
             (rec (cur prev)
               (incf iteration-count)
               (cond
                 ((> iteration-count *maximum-iterations*)
                  (warn "Performed maximum number of iterations.")
                  cur)
                 
                 ((almost-equal cur prev epsilon)
                  cur)
                 
                 (t
                  (rec (iteration cur) cur)))))

      (rec (iteration estimate)
           estimate))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Test ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-root (trials &optional (epsilon 0.00001))
  (loop :repeat trials
        :for rand := (random most-positive-fixnum)
          :then (random most-positive-fixnum)
        :for control := (sqrt rand)
        :for trial   := (square-root rand epsilon)
        :unless (almost-equal control trial epsilon)
          :collect (list control
                         trial
                         (abs (- control trial)))))