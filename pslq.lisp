;;;; pslq.lisp
;;;; Copyright (c) 2012 Robert Smith

;;;; An implementation of lattice reduction with the PSLQ algorithm.
;;;; 
;;;; Problem: Given a vector u in Rⁿ, find a vector v in Zⁿ such that
;;;; such that u.v = 0 and |v| is minimal.

(declaim (optimize (speed 0) safety debug))

#+clisp
(setf (ext:long-float-digits) 500)

;;;;;;;;;;;;;;;;;;;;; Matrix & Vector Arithmetic ;;;;;;;;;;;;;;;;;;;;;

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
  
  (define-vector-scalar-operation v/s #'/)
  
  (define-scalar-vector-operation s*v #'*)
  
  (define-pointwise-operation .+ #'+)
  (define-pointwise-operation .- #'-)
  (define-pointwise-operation .* #'*)
  (define-pointwise-operation ./ #'/)
  )

(defun square (x) 
  "Compute the square of X."
  (* x x))

(defun dist (a b)
  "Compute the norm of A and B (the length of the hypotenuse of a
right triangle with legs A and B)."
  (sqrt (+ (square a) (square b))))

(defun dot (u v)
  "Compute the dot product of vectors U and V."
  (reduce #'+ (map 'vector #'* u v)))

(defun norm (v)
  "Compute the norm (or magnitude) of the vector V."
  (sqrt (dot v v)))

(defun row-norm (m row)
  "Compute the norm of the ROWth row of M."
  (loop :for col :below (array-dimension m 1)
        :sum (square (aref m row col)) :into norm^2
        :finally (return (sqrt norm^2))))

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
  (make-array (list n m) :initial-element 0.0L0
                         :element-type 'long-float))

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
      (setf (aref m i i) 1.0L0))))

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
  (loop :for row :below (array-dimension m 0)
        :maximize (loop :for col :below (array-dimension m 1)
                        :maximize (aref m row col))))

(defun column (m col)
  "Return column COL of the matrix M."
  (loop :for row :below (array-dimension m 0)
        :collect (aref m row col)))

(defun float-exponent (f)
  "Compute the binary exponent of the floating point value F."
  (nth-value 1 (decode-float f)))

(defun min-max-exponent (vec)
  (loop :for x :across vec
        :minimizing (float-exponent x) :into mini
        :maximizing (float-exponent x) :into maxi
        :finally (return (values mini maxi))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PSLQ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *pslq-verbose* t
  "Switch to control the printing of extra information during lattice
  reduction.")

(defun find-integer-relation (x &key (tolerance (* 2 long-float-epsilon))
                                     (max-iterations nil))
  "Given a vector X of floating point values, attempt to find an
  integer relation vector Y such that

      X·Y = 0 ± TOLERANCE.

Perform up to MAX-ITERATIONS iterations, or infinitely many when null.
"
  (let ((gamma (sqrt (/ 4.0L0 3.0L0))) ; we must recompute this in
                                       ; case precision changes
        (n (length x))
        a b s y tt h bound)
    
    ;; Initialization
    
    ;; Step Init.1: Initialize matrices.

    (setf a (identity-matrix n)
          b (identity-matrix-int n))
    
    
    ;; Step Init.2: Initialize S and Y vector.

    (setf s (make-array n :initial-element 0.0L0))
    
    (dotimes (k n)
      (setf (aref s k)
            (sqrt (loop :for j :from k :below n
                        :sum (square (aref x j))))))
    
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
              (/ (aref s (1+ i))
                 (aref s i))))

      ;;                                y[i]y[j]
      ;; Lower triangle is H[i,j] = - ------------
      ;;                               s[j]s[j+1]
      (dotimes (j i)
        (setf (aref h i j)
              (- (/ (* (aref y i) (aref y j))
                    (* (aref s j) (aref s (1+ j))))))))
    
    
    ;; Step Init.4: Reduce H.

    (loop
      :for i :from 1 :below n
      :do (loop :for j :from (1- i) :downto 0
                :do (progn
                      (setf tt (round (aref h i j)
                                      (aref h j j)))
                      
                      (incf (aref y j) (* tt (aref y i)))
                      
                      (dotimes (k (1+ j))
                        (decf (aref h i k) (* tt (aref h j k))))
                      
                      (dotimes (k n)
                        (decf (aref a i k)
                              (* tt (aref a j k)))
                        
                        (incf (aref b k j)
                              (* tt (aref b k i)))))))
    
    ;; Loop
    
    (prog (m (iterations 0) (old nil))
       
     :start
       
       (incf iterations)
       
       ;; Step Loop.1: Compute M.
       
       (setf m (max-index
                (tr h :key (lambda (x i)
                             (* (expt gamma (1+ i))
                                (abs x))))))
       

       ;; Step Loop.2: Swap entries.
       
       (rotatef (aref y m) (aref y (1+ m)))
       (swap-rows a m (1+ m))
       (swap-rows h m (1+ m))
       (swap-cols b m (1+ m))

       
       ;; Step Loop.3
       
       (when (< m (- n 2))
         (let* ((t0 (dist (aref h m m)
                          (aref h m (1+ m))))
                (t1 (/ (aref h m m) t0))
                (t2 (/ (aref h m (1+ m)) t0))
                (t3 0.0L0)
                (t4 0.0L0))
           (loop
             :for i :from m :below n
             :do (progn
                   (setf t3 (aref h i m)
                         t4 (aref h i (1+ m)))
                   
                   (setf (aref h i m) (+ (* t1 t3)
                                         (* t2 t4))
                         (aref h i (1+ m)) (- (* t1 t4)
                                              (* t2 t3)))))))
       
       
       ;; Step Loop.4
       
       (loop
         :for i :from (1+ m) :below n
         :do (loop :for j :from (min (1- i) (1+ m)) :downto 0
                   :do (progn
                         (setf tt (round (aref h i j)
                                         (aref h j j)))
                         
                         (incf (aref y j) (* tt (aref y i)))
                         
                         (dotimes (k (1+ j))
                           (decf (aref h i k) (* tt (aref h j k))))
                         
                         (dotimes (k n)
                           (decf (aref a i k)
                                 (* tt (aref a j k)))
                           
                           (incf (aref b k j)
                                 (* tt (aref b k i)))))))
       
       
       ;; Step Loop.5
       
       (loop :for row :below (array-dimension h 0)
             :maximize (row-norm h row) :into max-norm
             :finally (setf bound (/ max-norm)))
       
       
       ;; Step Loop.6
       
       ;; If Max(A) exceeds precision => bad
       ;; If Min(Y) less than threshold => relation detected!
       
       (let* ((max-a (max-entry a))
              (min-y-idx (max-index y :key 'abs :predicate '<))
              (min-y (aref y min-y-idx))
              (relation (column b min-y-idx))
              (new (min-max-exponent y)))
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
             (format t "R.X = ~A~%" (dot relation x)))
           
           ;(terpri)
           (force-output))
         
         
         
         (cond
           ;; XXX: Check Max(A)
           ((<= (abs min-y) tolerance)
            (return relation))
           
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
        :for x := 1 :then (* x a)
        :collect x :into coeffs
        :finally (return (find-integer-relation (coerce coeffs 'vector)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TESTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-tests ()
  ;; FIXME: Fails when precision is set to 1k bits.
  (assert (equal '(1 16 -4)
                 (find-integer-relation (vector (- pi)
                                                (atan (/ 5.0L0))
                                                (atan (/ 239.0L0))))))
  
  (assert (equal '(1 0 -10 0 1)
                 (find-poly (+ (sqrt 2.0L0) (sqrt 3.0L0)) 4)))
  
  ;; This needs at least 326 iterations at 100 digits of precision
  (assert (equal '(-576 0 960 0 -352 0 40 0 -1)
                 (find-poly (+ (sqrt 2.0L0) 
                               (sqrt 3.0L0)
                               (sqrt 5.0L0))
                            8)))
  (format t "~&All tests passed!~%")
  t)

(defun x^n-1 (n)
  (find-poly (expt 2.0L0 (/ (float n 1.0L0))) n))
