;;; An experimental test to see if the solutions are correct
;;;
;;; Recurrences from THEOROS

(defun test (m n x0 y0)
  (labels (
           ;; Recurrences
           (x (k)
             (if (zerop k) x0
                 (+ (* (x (1- k))
                       (- 1 m))
                    (* n (y (1- k))))))
           (y (k)
             (if (zerop k) y0
                 (+ (* (y (1- k))
                       (- 1 n))
                    (* m (x (1- k))))))
           
           ;; Solutions
           (x-sol (k)
             (/ (+ (* n (+ x0 y0))
                   (* (expt (- 1 m n) k)
                      (- (* m x0)
                         (* n y0))))
                (+ m n)))
           
           (y-sol (k)
             (/ (+ (* m (+ x0 y0))
                   (* (expt (- 1 m n) k)
                      (- (* n y0)
                         (* m x0))))
                (+ m n))))
    
    (dotimes (k 10 t)
      (let* ((delta-x (- (x k) (x-sol k)))
             (delta-y (- (y k) (y-sol k))))
        (assert (and (zerop delta-x)
                     (zerop delta-y))
                ()
                "Failed with k=~A, dX=~A, dY=~A" k delta-x delta-y)))))

(defun do-test (&optional (trials 1000))
  (dotimes (i trials t)
    (test (1+ (random most-positive-fixnum))
          (1+ (random most-positive-fixnum))
          (1+ (random most-positive-fixnum))
          (1+ (random most-positive-fixnum)))))