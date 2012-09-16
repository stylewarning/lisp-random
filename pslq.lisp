;;;; pslq.lisp
;;;; Copyright (c) 2012 Robert Smith

;;;; An implementation of the PSLQ algorithm.


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
               `(defun ,name (,s ,v)
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

(defun dot (u v)
  (reduce #'+ (map 'vector #'* u v)))

(defun norm (v)
  (sqrt (dot v v)))

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

(defun max-index (v)
  (loop :with max-idx := 0
        :and  max-val := (aref v 0)
        :for i :from 0
        :for x :across v
        :when (> x max-val)
          :do (setq max-idx i
                    max-val x)
        :finally (return max-idx)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PSLQ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

