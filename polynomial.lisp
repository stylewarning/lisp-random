;;;; polynomial.lisp
;;;; Copyright (c) 2013 Robert Smith

(defun normalize-poly (p)
  "Normalize the polynomial P."
  (let ((last (position-if-not #'zerop p :from-end t))
        (length (length p)))
    (cond
      ((null last) #(0))
      ((= (1- length) last) p)
      (t (subseq p 0 (1+ last))))))

(defun degree (p)
  "Compute the degree of a polynomial P."
  (let ((p (normalize-poly p)))
    (1- (length p))))

(defun zero-poly (length)
  (make-array length :initial-element 0))

(defun eval-poly (p x)
  "Evaluate the polynomial P at X."
  (reduce (lambda (next sum)
            (+ next (* x sum)))
          p
          :initial-value 0
          :from-end t))

(defun poly-neg (p)
  "Negate a polynomial P."
  (map 'vector (lambda (x) (- x)) p))

(defun poly-add (p q)
  (let ((l1 (length p))
        (l2 (length q)))
    (if (> l2 l1)
        (poly-add q p)
        (let ((sum (zero-poly l1)))
          (loop :for i :below l2
                :do (setf (aref sum i)
                          (+ (aref p i)
                             (aref q i))))
          (loop :for i :from l2 :below l1
                :do (setf (aref sum i)
                          (aref p i))
                :finally (return (normalize-poly sum)))))))

(defun poly-sub (p q)
  (poly-add p (poly-neg q)))

(defun poly-mul (p q)
  (let ((lp (length p))
        (lq (length q)))
    (if (> lq lp)
        (poly-mul q p)
        (let ((prod (zero-poly (* lp lq))))
          (labels ((add! (factor offset)
                     (loop :for i :from offset :below (+ offset lq)
                           :for j :from 0
                           :do (incf (aref prod i)
                                     (* factor (aref q j))))))
            (loop :for offset :from 0
                  :for i :below lp
                  :do (add! (aref p i) offset)
                  :finally (return (normalize-poly prod))))))))
