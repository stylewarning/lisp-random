;;;; interval arithmetic
;;;; Copyright (c) 2012 Robert Smith

(defstruct (interval (:conc-name)
                     (:constructor interval (left right))
                     (:print-function interval-printer))
  left
  right)

(defun interval-printer (x stream depth)
  (declare (ignore depth))
  (format stream "[~A, ~A]" (left x) (right x)))

(defmacro with-iv (iv (a b) &body body)
  "Destructuring mechanism for intervals."
  (let ((iv-once (gensym)))
    `(let ((,iv-once ,iv))
       (let ((,a (left ,iv-once))
             (,b (right ,iv-once)))
         ,@body))))

(defconstant +unit-interval+ (interval 0 1))

(defun zero-in (iv)
  "Is zero in IV?"
  (<= (left iv) 0 (right iv)))

(defun number-to-interval (n)
  (assert (rationalp n))
  (interval n n))

(defmacro define-monotonic-unary-interval-function (fn-name unary-fn)
  `(defun ,fn-name (x)
     (with-iv x (a b)
       (let ((@a (funcall ,unary-fn a))
             (@b (funcall ,unary-fn b)))
         (interval (min @a @b)
                   (max @a @b))))))

(defmacro define-binary-interval-function (fn-name binary-fn)
  `(defun ,fn-name (x y)
     (with-iv x (a b)
       (with-iv y (c d)
         (let ((a@c (funcall ,binary-fn a c))
               (a@d (funcall ,binary-fn a d))
               (b@c (funcall ,binary-fn b c))
               (b@d (funcall ,binary-fn b d)))
           (interval (min a@c a@d b@c b@d)
                     (max a@c a@d b@c b@d)))))))

(define-binary-interval-function iv+ '+)
(define-binary-interval-function iv- '-)
(define-binary-interval-function iv* '*)

(define-monotonic-unary-interval-function iv-neg '-)

;;; We need to specially handle division by zero.
(defun iv/ (x y)
  (if (zero-in y)
      (error 'division-by-zero :operation 'iv/
                               :operands (list x y))
      (with-iv x (a b)
        (with-iv y (c d)
          (let ((a/c (/ a c))
                (a/d (/ a d))
                (b/c (/ b c))
                (b/d (/ b d)))
            (interval (min a/c a/d b/c b/d)
                      (max a/c a/d b/c b/d)))))))

(defun iv-reciprocal (x)
  (if (zero-in x)
      (error 'division-by-zero :operation 'iv/
                               :operands (list x))
      (iv/ (interval 1 1) x)))

;;; Only works for integral powers, for now...
(defun iv-pow (x n)
  (assert (and (integerp n)
               (plusp n)))
  (with-iv x (a b)
    (cond
      ((oddp n) (interval (expt a n) (expt b n)))
      ((evenp n) (cond
                   ((>= a 0) (interval (expt a n) (expt b n)))
                   ((minusp b) (interval (expt b n) (expt a n)))
                   (t (interval 0 (max (expt a n) (expt b n)))))))))

;;; Interval Unions

(defun simplify-union (intervals &key (sorted nil))
  "Simplify a list of intervals INTERVALS, which act as a union."
  (labels ((rec (intervals current coalesced)
             (if (null intervals)
                 (nreverse (cons current coalesced))
                 (let ((interval (car intervals)))
                   (cond
                     ;; CURRENT : o------o
                     ;; INTERVAL:   o--o
                     ;; RESULT  : o------o
                     ((<= (right interval) (right current))
                      (rec (cdr intervals) current coalesced))
                     
                     ;; CURRENT : o-----o
                     ;; INTERVAL:    o-----o
                     ;; RESULT  : o--------o
                     ((<= (left interval) (right current))
                      (rec (cdr intervals)
                           (interval (left current)
                                     (right interval))
                           coalesced))
                     
                     ;; CURRENT : o---o
                     ;; INTERVAL:        o---o
                     ;; RESULT  : o---o  o---o
                     (t
                      (rec (cdr intervals)
                           interval
                           (cons current coalesced))))))))
    (let ((sorted (if sorted
                      intervals
                      (sort (copy-list intervals)
                            #'<
                            :key #'left))))
      (rec (cdr sorted)
           (car sorted)
           nil))))
