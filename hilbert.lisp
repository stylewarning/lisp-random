;;;; hilbert.lisp
;;;; Copyright (c) 2013 Robert Smith

;;;; Hilbert's space-filling curve computations in Lisp.

;;;; XXX: This needs a little clean-up.

(defconstant I #C(0 1))
(defconstant 1+I #C(1 1))

(declaim (inline half))
(defun half (x)
  (/ x 2))

(defparameter *cache* (make-hash-table :test 'equalp))

(defun clear-cache ()
  (setf *cache* (make-hash-table :test 'equalp)))

(defun hilbert-aux (tt a1 a0)
  (labels ((hilbert-core (n t4)
             (cond
               ((= n 0) (* I (half (- 1 (hilbert-aux (- 1 t4)
                                                     (- (* I (half a1)))
                                                     (+ a0 (half (* a1 I))))))))
               ((= n 1) (half (+ I (hilbert-aux t4
                                                (half a1)
                                                (+ a0 (half (* a1 I)))))))
               ((= n 2) (half (+ 1+I (hilbert-aux t4
                                                  (half a1)
                                                  (+ a0 (half (* a1 1+I)))))))
               ((= n 3) (1+ (half (* I (hilbert-aux (- 1 t4)
                                                    (half (* a1 I))
                                                    (+ a0 a1))))))
               ((= n 4) 1))))
    (let ((res (gethash tt *cache*)))
      (if res
          (funcall res a1 a0)
          (progn
            (setf (gethash tt *cache*)
                  (lambda (s1 s0)
                    (/ (- a0 s0) (- s1 a1))))
            (let* ((t4 (* 4 tt))
                   (n (floor t4))
                   (t4-n (- t4 n))
                   (new (hilbert-core n t4-n)))
              (setf (gethash tt *cache*)
                    (lambda (b1 b0)
                      (declare (ignore b1 b0))
                      new))
              new))))))

(defun hilbert (x)
  (clear-cache)
  (hilbert-aux x 1 0))


(defun range (start end step)
  (loop :for x :from start :to end :by step
        :collect x))

(defun bench (n &optional (upper 1))
  (loop :with bound := (/ (expt 4 n)) 
        :for k :from (half bound) :to upper :by bound
        :count (hilbert k)))

(defun bench2 (n &optional (upper 1/2))
  (loop :with bound := (/ (expt 4 n)) 
        :for k :from (- 1/4 (half bound)) :to upper :by bound
        :count (hilbert k)))
