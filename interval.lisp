;;;; interval arithmetic
;;;; Copyright (c) 2012 Robert Smith

(defstruct (interval (:conc-name)
                     (:constructor interval (left right)))
  left
  right)

(defmacro with-iv (iv (a b) &body body)
  "Destructuring mechanism for intervals."
  (let ((iv-once (gensym)))
    `(let ((,iv-once ,iv))
       (let ((,a (left ,iv-once))
             (,b (right ,iv-once)))
         ,@body))))

(defun zero-in (iv)
  "Is zero in IV?"
  (<= (left iv) 0 (right iv)))

(defun iv+ (x y)
  (with-iv x (a b)
    (with-iv y (c d)
      (let ((a+c (+ a c))
            (a+d (+ a d))
            (b+c (+ b c))
            (b+d (+ b d)))
        (interval (min a+c a+d b+c b+d)
                  (max a+c a+d b+c b+d))))))

(defun iv- (x y)
  (with-iv x (a b)
    (with-iv y (c d)
      (let ((a-c (- a c))
            (a-d (- a d))
            (b-c (- b c))
            (b-d (- b d)))
        (interval (min a-c a-d b-c b-d)
                  (max a-c a-d b-c b-d))))))

(defun iv* (x y)
  (with-iv x (a b)
    (with-iv y (c d)
      (let ((a*c (* a c))
            (a*d (* a d))
            (b*c (* b c))
            (b*d (* b d)))
        (interval (min a*c a*d b*c b*d)
                  (max a*c a*d b*c b*d))))))

(defun iv/ (x y)
  (if (zero-in y)
      (error (make-condition 'division-by-zero :operation 'iv/
                                               :operands (list x y)))
      (with-iv x (a b)
        (with-iv y (c d)
          (let ((a/c (/ a c))
                (a/d (/ a d))
                (b/c (/ b c))
                (b/d (/ b d)))
            (interval (min a/c a/d b/c b/d)
                      (max a/c a/d b/c b/d)))))))

(defun iv-pow (x n)
  (assert (and (integerp n)
               (plusp n)))
  (with-iv x (a b)
    (cond
      ((oddp n) (interval (expt a n) (expt b n)))
      ((evenp n) (cond
                   ((>= a 0) (interval (expt a n) (expt b n)))
                   ((minusp b) (interval (expt b n) (expt a n)))
                   (otherwise (interval 0 (max (expt a n) (expt b n)))))))))