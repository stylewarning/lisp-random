(defun simulate (n)
  (let ((numbers (make-array n :initial-element 2)))
    (loop :for r := (random n) :then (random n)
          :for element := (aref numbers r)
          :until (= 1 element)
          :do (setf (aref numbers r) 1)
          :finally (return (- n (count 2 numbers))))))
;; sum(k/N^k * k! * (N-1 choose k-1), k=1..N)

(defun factorial (n)
    (case n
      ((0 1) 1)
      (t (loop :for i :from 2 :to n
               :for fac := i :then (* i fac)
               :finally (return fac)))))

(defun choose (n k)
  (/ (factorial n)
     (* (factorial k)
        (factorial (- n k)))))

(defun answer (n)
  (loop :for k :from 1 :to n
        :sum (float (* (/ k (expt n k))
                       (factorial k)
                       (choose (1- n)
                               (1- k))))))
