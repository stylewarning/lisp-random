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

(defun poly-diff (p)
  "Compute the derivative of the polynomial P."
  (if (poly-zerop p)
      +zero+
      (loop :for i :from 1 :below (length p)
            :collect (* i (aref p i)) :into dp
            :finally (return (coerce dp 'vector)))))

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

;;; Tricker stuff

(defmacro domatrix ((i j mij m &optional result) &body body)
  (let ((once (gensym "ONCE-"))
        (rows (gensym "ROWS-"))
        (cols (gensym "COLS-")))
    `(let ((,once ,m))
       (destructuring-bind (,rows ,cols)
           (array-dimensions ,once)
         (loop :for ,i :below ,rows
               :do (loop :for ,j :below ,cols
                         :do (let ((,mij (aref ,once ,i ,j)))
                               ,@body))
               :finally (return ,result))))))

(defun sylvester-matrix (p q)
  "Compute the Sylvester matrix of polynomials P and Q."
  (let* ((deg-p (degree p))
         (deg-q (degree q))
         (r (+ deg-p deg-q))
         (syl (make-array (list r r) :initial-element 0)))
    (loop :for i :below deg-q
          :do (loop :for j :to deg-p
                    :do (setf (aref syl i (+ i j))
                              (aref p (- deg-p j)))))
    
    (loop :for i :below deg-p
          :for row := (+ i deg-q)
          :do (loop :for j :to deg-q
                    :do (setf (aref syl row (+ i j))
                              (aref q (- deg-q j)))))
    
    syl))

(defun submatrix (matrix row-from col-from row-to col-to)
  (let* ((w (1+ (- col-to col-from)))
         (h (1+ (- row-to row-from)))
         (result (make-array (list h w) :initial-element 0)))
    (loop :for row :from row-from :to row-to
          :for i :from 0
          :do (loop :for col :from col-from :to col-to
                    :for j :from 0
                    :do (setf (aref result i j)
                              (aref matrix row col)))
          :finally (return result))))



(defun minor-matrix (matrix row col)
  "Compute the minor matrix of the matrix MATRIX, which is the matrix
  resulting from deleting the ROWth row and COLth column."
  (flet ((norm-row (x)
           (if (>= x row)
               (1- x)
               x))
         (norm-col (x)
           (if (>= x col)
               (1- x)
               x)))
    (destructuring-bind (nrows ncols)
        (array-dimensions matrix)
      (let ((result (make-array (list (1- nrows)
                                      (1- ncols))
                                :initial-element 0)))
        (domatrix (i j mij matrix result)
          (unless (or (= i row)
                      (= j col))
            (setf (aref result (norm-row i)
                               (norm-col j))
                  mij)))))))

(defun determinant (matrix)
  (labels ((cofactor (matrix row col dim)
             (* (expt -1 (+ row col))
                (det (minor-matrix matrix row col) (1- dim))))
           
           (det (matrix dim)
             (if (= 2 dim)
                 (- (* (aref matrix 0 0)
                       (aref matrix 1 1))
                    (* (aref matrix 0 1)
                       (aref matrix 1 0)))
                 (loop :for i :below dim
                       :for mi0 := (aref matrix i 0)
                       :sum (if (zerop mi0)
                                0
                                (* mi0 (cofactor matrix i 0 dim)))))))
    (destructuring-bind (h w)
        (array-dimensions matrix)
      (assert (= h w) (matrix) "Matrix must be square. Given ~Ax~A." h w)
      (det matrix w))))

(defun resultant (p q)
  "Compute the resultant of the polynomials P and Q."
  (* (expt (lc p) (degree q))
     (expt (lc q) (degree p))
     (determinant (sylvester-matrix p q))))
