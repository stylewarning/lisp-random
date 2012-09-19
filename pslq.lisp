;;;; pslq.lisp
;;;; Copyright (c) 2012 Robert Smith

;;;; An implementation of the PSLQ algorithm.

(declaim (optimize (speed 0) safety debug))

#+clisp
(setf (ext:long-float-digits) 100)

;;;;;;;;;;;;;;;;;;;;; Matrix & Vector Arithmetic ;;;;;;;;;;;;;;;;;;;;;

(macrolet ((define-scalar-vector-operation (name binary-fn)
             (let ((v (gensym "V-"))
                   (s (gensym "S-"))
                   (x (gensym "X-")))
               `(defun ,name (,v ,s)
                  (map 'vector
                       (lambda (,x) (funcall ,binary-fn ,x ,s))
                       ,v))))
           
           (define-vector-scalar-operation (name binary-fn)
             (let ((v (gensym "V-"))
                   (s (gensym "S-"))
                   (x (gensym "X-")))
               `(defun ,name (,v ,s)
                  (map 'vector
                       (lambda (,x) (funcall ,binary-fn ,s ,x))
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

(defun square (x) (* x x))

(defun dist (a b)
  (sqrt (+ (square a) (square b))))

(defun dot (u v)
  (reduce #'+ (map 'vector #'* u v)))

(defun norm (v)
  (sqrt (dot v v)))

(defun row-norm (m row)
  (loop :for col :below (array-dimension m 1)
        :sum (square (aref m row col)) :into norm^2
        :finally (return (sqrt norm^2))))

(defun normalize (v)
  (v/s v (norm v)))

(defun iota (n)
  (coerce (loop :for i :from 1 :to n :collect i)
          'vector))

(defun map-indexed (f v)
  (map 'vector f v (iota (length v))))

(defun zero-matrix (n &optional (m n))
  (make-array (list n m) :initial-element 0.0L0
                         :element-type 'long-float))

(defun zero-matrix-int (n &optional (m n))
  (make-array (list n m) :initial-element 0
                         :element-type 'integer))

(defun diagonal-matrix (v)
  (let* ((n (length v))
         (m (zero-matrix n)))
    (dotimes (i n m)
      (setf (aref m i i)
            (aref v i)))))

(defun identity-matrix (n)
  (let ((m (zero-matrix n)))
    (dotimes (i n m)
      (setf (aref m i i) 1.0L0))))

(defun identity-matrix-int (n)
  (let ((m (zero-matrix-int n)))
    (dotimes (i n m)
      (setf (aref m i i) 1))))

(defun diagonal-elements (m &key key)
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
  (dotimes (col (array-dimension m 1) m)
    (rotatef (aref m row-a col)
             (aref m row-b col))))

(defun swap-cols (m col-a col-b)
  (dotimes (row (array-dimension m 0) m)
    (rotatef (aref m row col-a)
             (aref m row col-b))))

(defun max-entry (m)
  (loop :for row :below (array-dimension m 0)
        :maximize (loop :for col :below (array-dimension m 1)
                        :maximize (aref m row col))))

(defun column (m col)
  (loop :for row :below (array-dimension m 0)
        :collect (aref m row col)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PSLQ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar gamma (sqrt (/ 4.0L0 3.0L0)))

(defun pslq (x &key (threshold 1.0L-5))
  
  (let ((n (length x)) a b s y tt h bound)
    
    ;; Initialization
    ;; Step Init.1

    (setf a (identity-matrix n)
          b (identity-matrix-int n))
    
    
    ;; Step Init.2

    (setf s (make-array n :initial-element 0.0L0))
    
    (loop :for k :below n
          :do (setf (aref s k)
                    (sqrt (loop :for j :from k :below n
                                :sum (square (aref x j))))))
    
    (setf tt (aref s 0))
    (setf y (v/s x tt))
    (setf s (v/s s tt))
    
    ;; Step Init.3

    (setf h (zero-matrix n (1- n)))
    
    (loop
      :for i :below n
      :do (progn
            (loop
              :for j :from (1+ i) :below (1- n)
              :do (progn
                    (setf (aref h i j) 0.0L0)))

            (when (< i (1- n))
              (setf (aref h i i)
                    (/ (aref s (1+ i))
                       (aref s i))))

            (loop :for j :to (1- i)
                  :do (setf (aref h i j)
                            (- (/ (* (aref y i) (aref y j))
                                  (* (aref s j) (aref s (1+ j)))))))))
    
    ;; Step Init.4

    (loop
      :for i :from 1 :below n
      :do (progn
            (loop :for j :from (1- i) :downto 0
                  :do (progn
                        (setf tt (round (aref h i j)
                                        (aref h j j)))
                        
                        (incf (aref y j) (* tt (aref y i)))
                        
                        (loop :for k :to j ; ??
                              :do (decf (aref h i k) (* tt (aref h j k))))
                        
                        (loop :for k :below n
                              :do (progn
                                    (decf (aref a i k)
                                          (* tt (aref a j k)))
                                    
                                    (incf (aref b k j)
                                          (* tt (aref b k i)))))))))
    
    ;; Loop
    
    (prog (m)
       
     :start
       
       ;; Step Loop.1
       
       (setf m (max-index
                (diagonal-elements h :key (lambda (x i)
                                            (* (expt gamma i)
                                               (abs x))))))
       
       ;; Step Loop.2
       
       (rotatef (aref y m) (aref y (1+ m)))
       
       (swap-rows a m (1+ m))
       (swap-rows h m (1+ m))
       
       (swap-cols b m (1+ m))

       
       ;; Step Loop.3
       
       (when (< m (- n 2))              ; < or <= ?
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
         :do (progn
               (loop :for j :from (min (1- i) (1+ m)) :downto 0
                     :do (progn
                           (setf tt (round (aref h i j)
                                           (aref h j j)))
                           
                           (incf (aref y j) (* tt (aref y i)))
                           
                           (loop :for k :to j ; ??
                                 :do (decf (aref h i k) (* tt (aref h j k))))
                           
                           (loop :for k :below n
                                 :do (progn
                                       (decf (aref a i k)
                                             (* tt (aref a j k)))
                                       
                                       (incf (aref b k j)
                                             (* tt (aref b k i)))))))))
       
       
       ;; Step Loop.5
       
       (loop :for row :below (array-dimension h 0)
             :maximize (row-norm h row) :into max-norm
             :finally (setf bound (/ max-norm)))
       
       
       ;; Step Loop.6
       
       (let* ((max-a (max-entry a))
              (min-y (max-index y :key 'abs :predicate '<))
              (relation (reverse (column b min-y))))
         (format t "Max of A: ~A~%" max-a)
         (format t "Min of Y: Y[~A] = ~A~%"
                 min-y
                 (aref y min-y))
         (format t "Y = ~A~%" y)
         (format t "Matrix B:~%")
         (pprint b)
         (terpri)
         (format t "Relation R: ~A~%" (column b min-y))
         (format t "R.X = ~A~%" (dot relation x))
         (terpri))
       
       (break)
       
       (go :start))))

(defun find-poly (a degree)
  (loop :repeat (1+ degree)
        :for x := 1 :then (* x a)
        :collect x :into coeffs
        :finally (return (pslq (coerce coeffs 'vector)))))