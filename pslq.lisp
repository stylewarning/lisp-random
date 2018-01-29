;;;; pslq.lisp
;;;; Copyright (c) 2012 Robert Smith

;;;; An implementation of lattice reduction with the PSLQ algorithm.
;;;;
;;;; Problem: Given a vector u in Rⁿ, find a vector v in Zⁿ such that
;;;; such that u.v = 0 and |v| is minimal.

(require :sb-mpfr)

(declaim (optimize (speed 0) safety debug))

;;;;;;;;;;;;;;;;;;;;; Matrix & Vector Arithmetic ;;;;;;;;;;;;;;;;;;;;;

(defun mpfr (x)
  (sb-mpfr:coerce x 'sb-mpfr:mpfr-float))

(macrolet ((define-scalar-vector-operation (name binary-fn)
             (let ((v (gensym "V-"))
                   (s (gensym "S-"))
                   (x (gensym "X-")))
               `(defun ,name (,s ,v)
                  (map 'vector
                       (lambda (,x) (funcall ,binary-fn ,s ,x))
                       ,v))))

           (define-vector-scalar-operation (name binary-fn)
             (let ((v (gensym "V-"))
                   (s (gensym "S-"))
                   (x (gensym "X-")))
               `(defun ,name (,v ,s)
                  (map 'vector
                       (lambda (,x) (funcall ,binary-fn ,x ,s))
                       ,v))))

           (define-pointwise-operation (name binary-fn)
             (let ((u (gensym "U-"))
                   (v (gensym "V-")))
               `(defun ,name (,u ,v)
                  (map 'vector ,binary-fn ,u ,v)))))

  (define-vector-scalar-operation v/s #'sb-mpfr:div)

  (define-scalar-vector-operation s*v #'sb-mpfr:mul)

  (define-pointwise-operation .+ #'sb-mpfr:add)
  (define-pointwise-operation .- #'sb-mpfr:sub)
  (define-pointwise-operation .* #'sb-mpfr:mul)
  (define-pointwise-operation ./ #'sb-mpfr:div)
  )

(defun square (x)
  "Compute the square of X."
  (sb-mpfr:mul x x))

(defun dist (a b)
  "Compute the norm of A and B (the length of the hypotenuse of a
right triangle with legs A and B)."
  (sb-mpfr:sqrt (sb-mpfr:add (square a) (square b))))

(defun dot (u v)
  "Compute the dot product of vectors U and V."
  (reduce #'sb-mpfr:add (map 'vector #'sb-mpfr:mul u v)))

(defun norm (v)
  "Compute the norm (or magnitude) of the vector V."
  (sqrt (dot v v)))

(defun row-norm (m row)
  "Compute the norm of the ROWth row of M."
  (loop :with norm^2 := (mpfr 0)
        :for col :below (array-dimension m 1)
        :do (setf norm^2 (sb-mpfr:add norm^2 (square (aref m row col))))
        :finally (return (sb-mpfr:sqrt norm^2))))

(defun normalize (v)
  "Normalize the vector V."
  (v/s v (norm v)))

(defun iota (n)
  "Create a vector of length N containing 1 through N."
  (coerce (loop :for i :from 1 :to n :collect i)
          'vector))

(defun map-indexed (f v)
  "Map the binary function F across V with the second argument of F
being the index of the elements of V."
  (map 'vector f v (iota (length v))))

(defun zero-matrix (n &optional (m n))
  "Create an N by M floating-point zero matrix."
  (make-array (list n m) :initial-element (mpfr 0)
                         :element-type 'sb-mpfr:mpfr-float))

(defun zero-matrix-int (n &optional (m n))
  "Create an N by M integer zero matrix."
  (make-array (list n m) :initial-element 0
                         :element-type 'integer))

(defun diagonal-matrix (v)
  "Create a square matrix whose diagonal is the vector V."
  (let* ((n (length v))
         (m (zero-matrix n)))
    (dotimes (i n m)
      (setf (aref m i i)
            (aref v i)))))

(defun identity-matrix (n)
  "Create a floating-point identity matrix of size N."
  (let ((m (zero-matrix n)))
    (dotimes (i n m)
      (setf (aref m i i) (mpfr 1)))))

(defun identity-matrix-int (n)
  "Create an integer identity matrix of size N."
  (let ((m (zero-matrix-int n)))
    (dotimes (i n m)
      (setf (aref m i i) 1))))

(defun tr (m &key key)
  "Compute the trace of M, optionally mapping the binary function KEY
where the second argument is the index of the element in the trace."
  (unless key
    (setf key (lambda (x i)
                (declare (ignore i))
                x)))
  (loop :for i :below (min (array-dimension m 0)
                           (array-dimension m 1))
        :collect (funcall key (aref m i i) i) :into diags
        :finally (return (coerce diags 'vector))))

(defun max-index (v &key (key 'identity)
                         (predicate '>))
  "Compute the maximum value in V according to the predicate PREDICATE
and metric function KEY."
  (loop :with max-idx := 0
        :and  max-val := (funcall key (aref v 0))
        :for i :from 0
        :for x :across v
        :for xx := (funcall key x)
        :when (funcall predicate xx max-val)
          :do (setq max-idx i
                    max-val xx)
        :finally (return max-idx)))

(defun swap-rows (m row-a row-b)
  "Swap the values in rows ROW-A and ROW-B of the matrix M."
  (dotimes (col (array-dimension m 1) m)
    (rotatef (aref m row-a col)
             (aref m row-b col))))

(defun swap-cols (m col-a col-b)
  "Swap the values in columns COL-A and COL-B in the matrix M."
  (dotimes (row (array-dimension m 0) m)
    (rotatef (aref m row col-a)
             (aref m row col-b))))

(defun max-entry (m)
  "Find the maximum element in the matrix M."
  (loop :with max := (aref m 0 0)
        :for row :below (array-dimension m 0)
        :do (loop :for col :below (array-dimension m 1)
                  :for el := (aref m row col)
                  :do  (when (sb-mpfr:> el max)
                         (setf max el)))
        :finally (return max)))

(defun column (m col)
  "Return column COL of the matrix M."
  (loop :for row :below (array-dimension m 0)
        :collect (aref m row col)))

(defun float-exponent (f)
  "Compute the binary exponent of the floating point value F."
  (sb-mpfr:coerce  (sb-mpfr:log2 (sb-mpfr:abs f)) 'double-float)
  ;; (nth-value 1 (decode-float f))
  )

(defun min-max-exponent (vec)
  (loop :for x :across vec
        :minimizing (float-exponent x) :into mini
        :maximizing (float-exponent x) :into maxi
        :finally (return (values mini maxi))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PSLQ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *pslq-verbose* t
  "Switch to control the printing of extra information during lattice
  reduction.")

(defun find-integer-relation (x &key tolerance
                                     (max-iterations nil))
  "Given a vector X of floating point values, attempt to find an
  integer relation vector Y such that

      X·Y = 0 ± TOLERANCE.

Perform up to MAX-ITERATIONS iterations, or infinitely many when null.
"
  (setf tolerance
        (etypecase tolerance
          (null
           (sb-mpfr:mul-2-raised (mpfr 1) (- (max 1 (1- sb-mpfr:+mpfr-precision+)))))
          (real
           (sb-mpfr:coerce tolerance 'sb-mpfr:mpfr-float))
          (sb-mpfr:mpfr-float
           tolerance)))
  (when *pslq-verbose*
    (format t "Tolerance: ~A" tolerance))

  (let ((gamma (sb-mpfr:sqrt (sb-mpfr:div (mpfr 4) (mpfr 3))))
                                        ; we must recompute this in
                                        ; case precision changes
        (n (length x))
        a b s y tt h bound)

    ;; Initialization

    ;; Step Init.1: Initialize matrices.

    (setf a (identity-matrix n)
          b (identity-matrix-int n))


    ;; Step Init.2: Initialize S and Y vector.

    (setf s (make-array n :initial-element (mpfr 0)))

    (dotimes (k n)
      (setf (aref s k)
            (sb-mpfr:sqrt (loop :with ss := (mpfr 0)
                                :for j :from k :below n
                                :do (setf ss
                                          (sb-mpfr:add ss
                                                       (square (aref x j))))
                                :finally (return ss)))))

    (setf tt (aref s 0))
    (setf y (v/s x tt))
    (setf s (v/s s tt))


    ;; Step Init.3: Initialize H.

    (setf h (zero-matrix n (1- n)))

    (dotimes (i n)
      ;; Upper triangle = 0.

      ;; Diagonal is H[i,i] = s[i+1]/s[i]
      (when (< i (1- n))
        (setf (aref h i i)
              (sb-mpfr:div (aref s (1+ i))
                           (aref s i))))

      ;;                                y[i]y[j]
      ;; Lower triangle is H[i,j] = - ------------
      ;;                               s[j]s[j+1]
      (dotimes (j i)
        (setf (aref h i j)
              (sb-mpfr:negate (sb-mpfr:div (sb-mpfr:mul (aref y i) (aref y j))
                                           (sb-mpfr:mul (aref s j) (aref s (1+ j))))))))


    ;; Step Init.4: Reduce H.

    (loop
      :for i :from 1 :below n
      :do (loop :for j :from (1- i) :downto 0
                :do (progn
                      (setf tt (sb-mpfr:round (sb-mpfr:div (aref h i j)
                                                           (aref h j j))))

                      (setf (aref y j)
                            (sb-mpfr:add (aref y j)
                                         (sb-mpfr:mul tt (aref y i))))

                      (dotimes (k (1+ j))
                        (setf (aref h i k)
                              (sb-mpfr:sub (aref h i k)
                                           (sb-mpfr:mul tt (aref h j k)))))

                      (dotimes (k n)
                        (setf (aref a i k)
                              (sb-mpfr:sub (aref a i k)
                                           (sb-mpfr:mul tt (aref a j k))))

                        (incf (aref b k j)
                              (* (sb-mpfr:coerce tt 'integer)
                                 (aref b k i)))))))

    ;; Loop

    (prog (m (iterations 0) (old nil))

     :start

       (incf iterations)

       ;; Step Loop.1: Compute M.

       (setf m (max-index
                (tr h :key (lambda (x i)
                             (sb-mpfr:mul (sb-mpfr:power gamma (1+ i))
                                          (sb-mpfr:abs x))))
                :predicate 'sb-mpfr:>))


       ;; Step Loop.2: Swap entries.

       (rotatef (aref y m) (aref y (1+ m)))
       (swap-rows a m (1+ m))
       (swap-rows h m (1+ m))
       (swap-cols b m (1+ m))


       ;; Step Loop.3

       (when (< m (- n 2))
         (let* ((t0 (dist (aref h m m)
                          (aref h m (1+ m))))
                (t1 (sb-mpfr:div (aref h m m) t0))
                (t2 (sb-mpfr:div (aref h m (1+ m)) t0))
                (t3 (mpfr 0))
                (t4 (mpfr 0)))
           (loop
             :for i :from m :below n
             :do (progn
                   (setf t3 (aref h i m)
                         t4 (aref h i (1+ m)))

                   (setf (aref h i m) (sb-mpfr:add (sb-mpfr:mul t1 t3)
                                                   (sb-mpfr:mul t2 t4))
                         (aref h i (1+ m)) (sb-mpfr:sub (sb-mpfr:mul t1 t4)
                                                        (sb-mpfr:mul t2 t3)))))))


       ;; Step Loop.4

       (loop
         :for i :from (1+ m) :below n
         :do (loop :for j :from (min (1- i) (1+ m)) :downto 0
                   :do (progn
                         (setf tt (sb-mpfr:round (sb-mpfr:div (aref h i j)
                                                              (aref h j j))))

                         (setf (aref y j)
                               (sb-mpfr:add (aref y j)
                                            (sb-mpfr:mul tt (aref y i))))

                         (dotimes (k (1+ j))
                           (setf (aref h i k)
                                 (sb-mpfr:sub (aref h i k)
                                              (sb-mpfr:mul tt (aref h j k)))))

                         (dotimes (k n)
                           (setf (aref a i k)
                                 (sb-mpfr:sub (aref a i k)
                                              (sb-mpfr:mul tt (aref a j k))))

                           (incf (aref b k j)
                                 (* (sb-mpfr:coerce tt 'integer) (aref b k i)))))))


       ;; Step Loop.5

       (loop :with max-norm := (mpfr -1)
             :for row :below (array-dimension h 0)
             :do (let ((rn (row-norm h row)))
                   (when (sb-mpfr:> rn max-norm)
                     (setf max-norm rn)))
             :finally (setf bound (sb-mpfr:div (mpfr 1) max-norm)))


       ;; Step Loop.6

       ;; If Max(A) exceeds precision => bad
       ;; If Min(Y) less than threshold => relation detected!

       (let* ((max-a (max-entry a))
              (min-y-idx (max-index y :key 'sb-mpfr:abs :predicate 'sb-mpfr:<))
              (min-y (aref y min-y-idx))
              (relation (column b min-y-idx))
              (new (min-max-exponent y))
              (R.X (dot relation x)))
         (when *pslq-verbose*
           (format t "~6,' D: ~S~%" iterations relation)

           (progn
             (format t "Max of A: ~A~%" max-a)
             (format t "Min of Y: Y[~A] = ~A~%"
                     min-y-idx
                     min-y)
             (format t "Y = ~A~%" y)
             (format t "Norm: ~A~%" bound)
             (multiple-value-bind (mini maxi) (min-max-exponent y)
               (format t "MAX/MIN = ~A~%" (float (/ maxi mini))))
             (format t "Relation R: ~A~%" relation)
             (format t "R.X = ~A~%" R.X))

                                        ;(terpri)
           (force-output))



         (cond
           ;; XXX: Check Max(A)
           ((sb-mpfr:<= (sb-mpfr:abs min-y) tolerance)
            (return (values relation R.X)))

           #+#:ignore
           ((and old (>= (* 0.5 (abs new)) (abs old)))
            (warn "Probable relation found: ~S" relation)
            (if (y-or-n-p "Continue?")
                (go :start)
                (return relation)))

           ((and max-iterations
                 (>= iterations max-iterations))
            (progn
              (when *pslq-verbose*
                (format t "Max iterations exceeded."))
              nil))

           (t (progn
                (setf old new)
                (go :start))))))))

(defun find-poly (a degree)
  "Find a polynomial p of degree DEGREE such that A is a root."
  (loop :repeat (1+ degree)
        :for x := (mpfr 1) :then (sb-mpfr:mul x a)
        :collect x :into coeffs
        :finally (return (find-integer-relation (coerce coeffs 'vector)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TESTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun x^n-1 (x n)
  (find-poly (sb-mpfr:power (mpfr x) (mpfr (/ n))) n))

(defun run-tests (&optional (prec 100 #+ignore sb-mpfr:+mpfr-precision+))
  (check-type prec (integer 1))
  (sb-mpfr:with-precision prec
    ;; FIXME: Fails when precision is set to 1k bits.
    (assert (equal '(1 16 -4)
                   (find-integer-relation (vector (sb-mpfr:negate (sb-mpfr:const-pi))
                                                  (sb-mpfr:atan (sb-mpfr:div (mpfr 1) (mpfr 5)))
                                                  (sb-mpfr:atan (sb-mpfr:div (mpfr 1) (mpfr 239)))))))

    (assert (equal '(1 0 -10 0 1)
                   (find-poly (sb-mpfr:add (sb-mpfr:sqrt (mpfr 2))
                                           (sb-mpfr:sqrt (mpfr 3)))
                              4)))

    ;; This needs at least 326 iterations at 100 digits of precision
    (assert (equal '(-576 0 960 0 -352 0 40 0 -1)
                   (find-poly (sb-mpfr:add
                               (sb-mpfr:sqrt (mpfr 2))
                               (sb-mpfr:add
                                (sb-mpfr:sqrt (mpfr 3))
                                (sb-mpfr:sqrt (mpfr 5))))
                              8)))

    (loop :for n :from 2 :to 5
          :for p := (x^n-1 2 n)
          :do (assert (and (= 2 (first p))
                           (= -1 (first (last p)))))))
  (format t "~&All tests passed!~%")
  t)
