;;;; fib.lisp
;;;; Copyright (c) 2012 Robert Smith

;;; Dies at 42
(defun slow-fib (n)
  (cond
    ((zerop n) 0)
    ((= 1 n)   1)
    (t (+ (slow-fib (- n 1))
          (slow-fib (- n 2))))))

;;; Dies at 42000
(defun fib-memo (n)
  (let ((memo (make-array (1+ n) :initial-element nil)))
    
    ;; Base Cases
    (setf (aref memo 0) 0
          (aref memo 1) 1)
    
    ;; General Case - Top-Down Recursion
    (labels ((rec (n)
               (let ((ref (aref memo n)))
                 (or ref
                     (setf (aref memo n)
                           (+ (rec (- n 1))
                              (rec (- n 2))))))))
      (rec n))))

(defun fib-dyn (n)
  (let ((memo (make-array (1+ n) :initial-element nil)))
    
    ;; Base Case
    (setf (aref memo 0) 0
          (aref memo 1) 1)

    ;; General Case - Bottom-Up Iteration
    (loop :for i :from 2 :to n
          :do (setf (aref memo i)
                    (+ (aref memo (- i 1))
                       (aref memo (- i 2))))
          :finally (return (aref memo n)))))