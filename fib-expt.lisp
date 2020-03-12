(defun mul (a b c d                     ; 2x2 matmul
            e f g h)
  ;; [a b] [e f]
  ;; [c d] [g h]
  (values (+ (* a e) (* b g))
          (+ (* a f) (* b h))
          (+ (* c e) (* d g))
          (+ (* c f) (* d h))))

(defun sq (a b c d)
  (mul a b c d
       a b c d))

(defun pow (a b c d n)
  ;; [a b]
  ;; [c d]
  ;; to the power of n
  (cond
    ((zerop n) (values 1 0 0 1))
    ((= 1 n)   (values a b c d))
    ((evenp n)
     (multiple-value-call #'pow (sq a b c d) (/ n 2)))
    ((oddp n)
     (multiple-value-call #'mul
       a b c d
       (pow a b c d (1- n))))))

(defun fib (n)
  (nth-value 1 (pow 0 1 1 1 n)))
