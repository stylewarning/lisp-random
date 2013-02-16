;;;; polynomial.lisp
;;;; Copyright (c) 2013 Robert Smith

(defvar +zero+ #(0))

(defun normalize-poly (p)
  "Normalize the polynomial P."
  (let ((last (position-if-not #'zerop p :from-end t))
        (length (length p)))
    (cond
      ((null last) +zero+)
      ((= (1- length) last) p)
      (t (subseq p 0 (1+ last))))))

(defun degree (p)
  "Compute the degree of a polynomial P."
  (let ((p (normalize-poly p)))
    (1- (length p))))

(defun zero-poly (length)
  (make-array length :initial-element 0))

(defun poly-zerop (p)
  (every #'zerop p))

(defun eval-poly (p x)
  "Evaluate the polynomial P at X."
  (reduce (lambda (next sum)
            (+ next (* x sum)))
          p
          :initial-value 0
          :from-end t))

(defun lc (p)
  "Get the leading coefficient."
  (aref p (1- (length p))))

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

(defun poly-scale (p factor)
  "Scale the polynomial P by the factor FACTOR."
  (if (zerop factor)
      +zero+
      (map 'vector (lambda (c)
                     (* x factor))
           p)))

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

(defun poly-div (n d)
  (if (poly-zerop d)
      (error 'division-by-zero :operands (list n d)
                               :operation '/)
      (loop :with q := +zero+
            :and  r := n
            :and  temp := 0
            :while (and (not (poly-zerop r))
                        (>= (degree r)
                            (degree d)))
            ;; this is not correct.
            :do (let ((next (/ (lc r) (lc d))))
                  (psetf q (+ (aref q 0)) (+ q next)
                         r (- r (* next d))))
            :finally (return (values q r)))))
