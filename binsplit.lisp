;;;; binsplit.pdf
;;;; Copyright (c) 2012 Robert Smith

(defun print-unreadable (object stream depth)
  (declare (ignore depth))
  (print-unreadable-object (object stream :type t :identity t)))

(defun square (x) (* x x))

(defun cube (x) (* x x x))

(defstruct (binsplit-series (:conc-name series.)
                            (:print-function print-unreadable))
  a b p q)

(defstruct (binsplit-computation (:conc-name comp.))
  (p 0 :type integer)
  (q 0 :type integer)
  (b 0 :type integer)
  (r 0 :type integer))

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
;;; also buggy
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

(defvar pi-series (let* ((chud-a 13591409)
                         (chud-b 545140134)
                         (chud-c 640320)
                         (chud-c^3 (/ (cube chud-c) 24)))
                    (flet ((a (n) (+ chud-a (* chud-b n)))
                           (p (n) (if (zerop n)
                                      1
                                      (* -1
                                         (- (* 6 n) 5)
                                         (- (* 2 n) 1)
                                         (- (* 6 n) 1))))
                           (q (n) (if (zerop n)
                                      1
                                      (* n n n chud-c^3))))
                      (make-binsplit-series :a #'a
                                            :b (constantly 1)
                                            :p #'p
                                            :q #'q))))

(defconstant +chud-decimals-per-term+ 14.181647462d0)

(defun compute-pi (prec)
  (let* ((chud-c 640320)
         (chud-c/12 (/ chud-c 12))
         (num-terms (floor (+ 2 (/ prec +chud-decimals-per-term+))))
         (sqrt-c (isqrt (* chud-c (expt 100 prec)))))
    (values (floor  (* sqrt-c chud-c/12)
                    (compute-series pi-series :upper num-terms)))))