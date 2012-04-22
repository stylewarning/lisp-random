;;;; m-of-n-circuit.lisp
;;;; Copyright (c) 2011 Robert Smith

(defun any-1-of-3 (a b c)
  (or a b c))

(defun any-2-of-3 (a b c)
  (if a (or b c) (and b c)))

(defun any-3-of-3 (a b c)
  (and a b c))

;;; We could optimalize to check that there are >= M inputs.
(defun any-m-of-n (m inputs)
  (cond
    ((zerop m) t)
    ((null inputs) nil)
    ((car inputs) (any-m-of-n (1- m) (cdr inputs)))
    (t (any-m-of-n m (cdr inputs)))))

;;; Here we keep track of the number of inputs so we can fail quickly.
(defun any-m-of-n-quickfail (m inputs)
  (labels ((any (m len inputs)
             (cond
               ((zerop m) t)
               ((> m len) nil)
               ((car inputs) (any (1- m) (1- len) (cdr inputs)))
               (t (any m (1- len) (cdr inputs))))))
    (any m (length inputs) inputs)))

(defun build-circuit (m inputs)
  (labels ((any (m len inputs)
             (cond
               ((zerop m) t)
               ((> m len) nil)
               ((and (= 1 m)
                     (< 1 len)) `(or ,@inputs))
               ((= 1 m len) (car inputs))
               ((= m len) `(and ,@inputs))
               (t `(if ,(car inputs)
                       ,(any (1- m) (1- len) (cdr inputs))
                       ,(any m (1- len) (cdr inputs)))))))
    (any m (length inputs) inputs)))
