;;;; stairs.lisp
;;;; Copyright (c) 2012 Robert Smith

;;;; Given 10 stairs, and given one can take between 1 and 3 steps,
;;;; how many ways can you reach the top of the stairs?

;;; This question is equivalent to: How many ways can we sum to 5
;;; using {1, 2, 3}.

(defun stairs (a b)
  (assert (> b a))
  
  (cond
    ((> a b) 0)
    ((= a b) 1)
    ((< a b) (+ (stairs (+ a 1) b)
                (stairs (+ a 2) b)
                (stairs (+ a 3) b)))))



(defun stairs* (a b)
  (assert (> b a))

  (labels ((rec (x)
             (cond
               ((minusp x) 0)
               ((zerop x)  1)
               (t (+ (rec (- x 1))
                     (rec (- x 2))
                     (rec (- x 3)))))))
    (rec (- b a))))

(defun stairs-memo (a b)
  (assert (> b a))
  (let ((memo (make-array (1+ (- b a)) :initial-element nil)))
    (setf (aref memo 0) 1)
    
    (labels ((rec (x)
               (if (minusp x) 0
                   (let ((ref (aref memo x)))
                     (or ref
                         (setf (aref memo x)
                               (+ (rec (- x 1))
                                  (rec (- x 2))
                                  (rec (- x 3)))))))))
      (rec (- b a)))))


