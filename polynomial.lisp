;;;; polynomial.lisp
;;;; Copyright (c) 2013 Robert Smith

(defvar +zero+ #(0))

(defstruct (term (:constructor term (coefficient exponent))
                 (:predicate termp))
  (coefficient 0 :type real)
  (exponent 0 :type integer))

(defun normalize-poly (p)
  "Normalize the polynomial P."
  (let ((last (position-if-not #'zerop p :from-end t))
        (length (length p)))
    (cond
      ((null last) +zero+)
      ((= (1- length) last) p)
      (t (subseq p 0 (1+ last))))))

(defun copy-poly (p)
  (copy-seq p))

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

;; XXX LENGTH COULD BE BUGGY
(defun lc (p)
  "Get the leading coefficient."
  (aref p (1- (length p))))

(defun leading-term (p)
  (let ((d (degree p)))
    (term (aref p d) d)))

(defun poly-neg (p)
  "Negate a polynomial P."
  (map 'vector (lambda (x) (- x)) p))

(defun poly-add-term (p term)
  (let ((coef (term-coefficient term))
        (exponent (term-exponent term)))
    (if (< exponent
           (length p)) ; This is an optimization for non-normalized P
        (let ((sum (copy-poly p)))
          (setf (aref sum exponent)
                (+ (aref sum exponent)
                   coef))
          sum)
        (let ((sum (make-array (1+ exponent) :initial-element 0)))
          (replace sum p)
          (setf (aref sum exponent)
                (+ (aref sum exponent)
                   coef))
          sum))))

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
                     (* c factor))
           p)))

(defun poly-mul-term (p term)
  (let ((coef (term-coefficient term))
        (exponent (term-exponent term)))
    (if (zerop exponent)
        (poly-scale p coef)
        (let ((prod (zero-poly (+ 1 (degree p)
                                  exponent))))
          (loop :for i :from exponent
                :for c :across p
                :do (setf (aref prod i)
                          (* coef c))
                :finally (return prod))))))

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
      (flet ((lt-quotient (a b)
               (let ((la (leading-term a))
                     (lb (leading-term b)))
                 (term (/ (term-coefficient la)
                          (term-coefficient lb))
                       (- (term-exponent la)
                          (term-exponent lb))))))
        (loop :with q := +zero+
              :and  r := n
              :while (and (not (poly-zerop r))
                          (>= (degree r)
                              (degree d)))
              :do (let ((next (lt-quotient r d)))
                    (psetf q (poly-add-term q next)
                           r (poly-sub r (poly-mul-term d next))))
              :finally (return (values q r))))))
