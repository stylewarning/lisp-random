;;;; binsplit.pdf
;;;; Copyright (c) 2012 Robert Smith

(declaim (optimize speed))

(defun print-unreadable (object stream depth)
  (declare (ignore depth))
  (print-unreadable-object (object stream :type t :identity t)))

(declaim (inline constantly-integer))
(defun constantly-integer (k)
  (declare (type integer k))
  (lambda (n)
    (declare (type integer n)
             (ignore n))
    k))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline square cube))
  (defun square (x)
    (declare (type integer x))
    (* x x))

  (defun cube (x)
    (declare (type integer x))
    (* x x x)))

(deftype Z->Z () '(function (integer) integer))

(defstruct (binsplit-series (:conc-name series.)
                            (:print-function print-unreadable))
  (a (constantly-integer 1) :type Z->Z :read-only t)
  (b (constantly-integer 1) :type Z->Z :read-only t)
  (p (constantly-integer 1) :type Z->Z :read-only t)
  (q (constantly-integer 1) :type Z->Z :read-only t))

(defstruct (binsplit-computation (:conc-name comp.))
  (p 0 :type integer :read-only t)
  (q 0 :type integer :read-only t)
  (b 0 :type integer :read-only t)
  (r 0 :type integer :read-only t))

(defun product (f lower upper)
  "Compute the product
 
  F(LOWER) * F(LOWER + 1) * ... * F(UPPER - 1)."
  (declare (type integer lower upper)
           (type (function (integer) integer) f))
  (labels ((rec (current accum)
             (declare (type integer current accum))
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
  (declare (ignore upper)
           (type binsplit-series series)
           (type integer lower upper))
  (make-binsplit-computation :p (funcall (series.p series) lower)
                             :q (funcall (series.q series) lower)
                             :b (funcall (series.b series) lower)
                             :r (* (funcall (series.a series) lower)
                                   (funcall (series.p series) lower))))

(defun binary-split-base-case=n (series lower upper)
  (declare (type binsplit-series series)
           (type integer lower upper))
  (let ((b (product (series.b series) lower upper))
        (q (product (series.q series) lower upper)))
    (make-binsplit-computation :p (product (series.p series) lower upper)
                               :q q
                               :b b
                               :r (* b q (sum-series-direct series lower upper)))))

(defun combine-computations (comp-left comp-right)
  (declare (type binsplit-computation comp-left comp-right))
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
  (declare (type binsplit-series series)
           (type integer lower upper))
  (let ((delta (- upper lower)))
    (cond
      ((zerop delta) (error "UPPER must be greater than LOWER."))
      ((= 1 delta) (binary-split-base-case=1 series lower upper))
      #+#:bug
      ((< delta 5) (binary-split-base-case=n series lower upper))
      (t (let* ((m     (floor (+ lower upper) 2))
                (left  (binary-split series lower m))
                (right (binary-split series m upper)))
           (combine-computations left right))))))

(defun computation-numerator/denominator (comp)
  (values (comp.r comp)
          (* (comp.b comp) (comp.q comp))))

(defun computation-to-rational (comp)
  (/ (comp.r comp) (comp.b comp) (comp.q comp)))

(defun computation-to-integer (comp digits)
  (let ((ten (expt 10 digits)))
    (values (floor (* (comp.r comp) ten)
                   (* (comp.b comp) (comp.q comp))))))

(defun compute-series (series &key (lower 0) (upper 1000))
  (declare (type integer lower upper))
  (computation-to-rational (binary-split series lower upper)))


;;;;;;;;;;;;;;;;;;;;;;;;;;; Exponential eˣ ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun exp-series (x)
  (assert (rationalp x) (x) "X must be rational, given ~A." x)
  (make-binsplit-series :a (constantly-integer 1)
                        :b (constantly-integer 1)
                        :p (lambda (n) (if (zerop n) 1 (numerator x)))
                        :q (lambda (n) (if (zerop n) 1 (* n (denominator x))))))

(defvar e-series (exp-series 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; pi π ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +chud-decimals-per-term+ 14.181647462d0)
(defconstant +chud-a+ 13591409)
(defconstant +chud-b+ 545140134)
(defconstant +chud-c+ 640320)

(defvar pi-series (flet ((a (n)
                           (declare (type integer n))
                           (+ +chud-a+ (* n +chud-b+)))
                         (p (n)
                           (declare (type integer n))
                           (if (zerop n)
                               1
                               ;; This is Horner's form of
                               ;; -(6n - 5)*(2n - 1)*(6n - 1)
                               (+ 5 (* n (+ -46 (* n (+ 108 (* n -72))))))))
                         (q (n)
                           (declare (type integer n))
                           (if (zerop n)
                               1
                               (* (cube n)
                                  #.(/ (cube +chud-c+) 24)))))
                    (make-binsplit-series :a #'a
                                          :b (constantly-integer 1)
                                          :p #'p
                                          :q #'q)))

(defun compute-pi (prec)
  (let* ((num-terms (floor (+ 2 (/ prec +chud-decimals-per-term+))))
         ;; √640320 = 8√10005
         (sqrt-c    (* 8 (isqrt (* 10005 (expt 100 prec)))))
         (comp      (binary-split pi-series 0 num-terms)))
    (multiple-value-bind (num den) (computation-numerator/denominator comp)
      (values (floor (* sqrt-c den #.(/ +chud-c+ 12))
                     num)))))

;;;;;;;;;;;;;;;;;;;;;;;; Catalan's Constant G ;;;;;;;;;;;;;;;;;;;;;;;;

;;; This wouldn't actually compute Catalan's constant G. It would
;;; compute G' such that
;;;
;;;         3      π
;;;     G = - G' + - log(2 + √3)
;;;         8      8
;;;
(defvar catalan-series (make-binsplit-series :a (constantly-integer 1)
                                             :b (lambda (n)
                                                  (1+ (* 2 n)))
                                             :p (lambda (n)
                                                  (if (zerop n) 1 n))
                                             :q (lambda (n) 
                                                  (if (zerop n)
                                                      1
                                                      (+ 2 (* 4 n))))))


;;;;;;;;;;;;;;;;;;;;;;; Apéry's Constant ζ(3) ;;;;;;;;;;;;;;;;;;;;;;;;

;;; Actually computes 2*ζ(3).

(defvar apery-series (make-binsplit-series :a (lambda (n)
                                                (+ 77 (* n (+ 250 (* n 205)))))
                                           :b (constantly-integer 1)
                                           :p (lambda (n)
                                                (if (zerop n)
                                                    1
                                                    (- (* (square n)
                                                          (cube n))))) ; n⁵
                                           :q (lambda (n)
                                                (let ((2n+1 (1+ (* 2 n))))
                                                  (* 32 ; 32(2n+1)⁵
                                                     (square 2n+1)
                                                     (cube 2n+1))))))

(defun compute-apery (terms)
  (/ (compute-series apery-series :upper terms) 2))
