;;;; binsplit.pdf
;;;; Copyright (c) 2012 Robert Smith

(defstruct (binsplit-series (:conc-name series.))
  a b p q)

(defstruct (binsplit-computation (:conc-name comp.))
  p q b r)

(defun product (f lower upper)
  "Compute the product
 
  F(LOWER) * F(LOWER + 1) + ... + F(UPPER - 1)."
  (labels ((rec (current accum)
             (if (>= current upper)
                 accum
                 (rec (1+ current)
                      (* accum (funcall f current))))))
    (rec lower 1)))

;;; naive, slow direct summation
(defun sum-series-direct (series lower upper)
  (with-accessors ((a series.a)
                   (b series.b)
                   (p series.p)
                   (q series.q)) series
    (loop :for n :from lower :below upper
          :sum (* (/ (funcall a n) (funcall b n))
                  (/ (product p lower n)
                     (product q lower n))))))

(defun binary-split-base-case=1 (series lower upper)
  (declare (ignore upper))
  (make-binsplit-computation :p (funcall (series.p series) lower)
                             :q (funcall (series.q series) lower)
                             :b (funcall (series.b series) lower)
                             :r (* (funcall (series.a series) lower)
                                   (funcall (series.p series) lower))))

;;; buggy
(defun binary-split-base-case=n (series lower upper)
  (let ((b (product (series.b series) lower upper))
        (q (product (series.q series) lower upper)))
    (make-binsplit-computation :p (product (series.p series) lower upper)
                               :q q
                               :b b
                               :r (* b q (sum-series-direct series lower upper)))))

(defun combine-computations (comp-left comp-right)
  (with-accessors ((pl comp.p)
                   (ql comp.q)
                   (bl comp.b)
                   (rl comp.r)) comp-left
    (with-accessors ((pr comp.p)
                     (qr comp.q)
                     (br comp.b)
                     (rr comp.r)) comp-right
      (make-binsplit-computation :p (* pl pr)
                                 :q (* ql qr)
                                 :b (* bl br)
                                 :r (+ (* br qr rl)
                                       (* bl pl rr))))))

(defun binary-split (series lower upper)
  (let ((delta (- upper lower)))
    (cond
      ((zerop delta) (error "UPPER must be greater than LOWER."))
      ((= 1 delta) (binary-split-base-case=1 series lower upper))
      #+#:bug
      ((< delta 5) (binary-split-base-case=n series lower upper))
      (t (let* ((m    (floor (+ lower upper) 2))
                (left (binary-split series lower m))
                (right (binary-split series m upper)))
           (combine-computations left right))))))

(defun computation-to-rational (comp)
  (/ (comp.r comp) (comp.b comp) (comp.q comp)))

(defun computation-to-integer (comp digits)
  (let ((ten (expt 10 digits)))
    (values (floor (* (comp.r comp) ten)
                   (* (comp.b comp) (comp.q comp))))))

(defun compute-series (series &key (lower 0) (upper 1000))
  (computation-to-rational (binary-split series lower upper)))

(defun exp-series (x)
  (assert (rationalp x) (x) "X must be rational, given ~A." x)
  (make-binsplit-series :a (constantly 1)
                        :b (constantly 1)
                        :p (lambda (n) (if (zerop n) 1 (numerator x)))
                        :q (lambda (n) (if (zerop n) 1 (* n (denominator x))))))

(defvar e-series (exp-series 1))

