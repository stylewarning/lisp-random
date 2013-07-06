;;;; map-product.lisp
;;;; Copyright (c) 2013 Robert Smith

(defun incrementer (signature)
  (let* ((length (length signature))
         (state (make-array length :initial-element 0))
         (sig (coerce signature 'vector)))
    ;; So we get zeros for our first state.
    (setf (aref state 0) -1)
    
    ;; Return a generator.
    (lambda ()
      (loop :with carry := 1
            :for i :below length
            :do (if (< (+ carry (aref state i))
                       (aref sig i))
                    (progn
                      (incf (aref state i) carry)
                      (setf carry 0)) ; optimize by returning early?
                    (progn
                      (setf (aref state i) 0)
                      (setf carry 1)))
            :finally (return (if (plusp carry)
                                 nil
                                 state))))))

(defun exhaust-incrementer (sig)
  (let ((inc (incrementer sig)))
    (time
     (loop :for idx := (funcall inc) :then (funcall inc)
           :while idx
           :do (format t "~A~%" idx)
           :finally (return idx)))))

(defun map-product (f seqs)
  (let ((increment (incrementer (mapcar #'length seqs))))
    (loop :for idx := initial :then (funcall increment idx)
          :while idx
          :do (funcall f (mapcar #'elt seqs idx)))))
