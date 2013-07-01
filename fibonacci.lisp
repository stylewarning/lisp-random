;;;; fibonacci.lisp
;;;; Copyright (c) 2013 Robert Smith

;;;; Fast ways to compute Fibonacci and Fibonacci of powers of 2.

(declaim (optimize speed (safety 0) (debug 0) (space 0)))
(defun fib (n)
  (declare (type unsigned-byte n))
  (loop :with a :of-type unsigned-byte := 1
        :and  b :of-type unsigned-byte := 0
        :and  p :of-type unsigned-byte := 0
        :and  q :of-type unsigned-byte := 1
        :until (zerop n)
        :do (if (oddp n)
                (let ((a*q (* a q)))
                  (psetq a (+ (* b q)
                              a*q
                              (* a p))
                         b (+ (* b p)
                              a*q)
                         n (1- n)))
                (let ((q^2 (* q q)))
                  (psetq p (+ (* p p) q^2)
                         q (+ (* 2 p q) q^2)
                         n (ash n -1))))
        :finally (return b)))

(defun fib-rec (n)
  (declare (type unsigned-byte n))
  (labels ((rec (a b p q n)
             (declare (type unsigned-byte a b p q n))
             (the unsigned-byte
                  (cond
                    ((zerop n) b)
                    ((evenp n) (let ((q^2 (* q q)))
                                 (rec a
                                      b
                                      (+ (* p p) q^2)
                                      (+ (* 2 p q) q^2)
                                      (ash n -1))))
                    (t (let ((a*q (* a q)))
                         (rec (+ (* b q) a*q (* a p))
                              (+ (* b p) a*q)
                              p
                              q
                              (1- n))))))))
    (rec 1 0 0 1 n)))

(defun fib-rec-pow2 (n)
  (declare (type unsigned-byte n))
  (labels ((rec (p q n)
             (if (= 1 n)
                 q
                 (let ((q^2 (* q q)))
                   (rec (+ (* p p) q^2)
                        (+ (* 2 p q) q^2)
                        (ash n -1))))))
    (if (zerop n)
        1
        (rec 0 1 n))))

(defun fib-pow2 (n)
  (labels ((rec (m r n)
             (if (zerop n)
                 r
                 (rec (- (* m m) 2)
                      (* r m)
                      (ash n -1)))))
    (if (or (= n 1)
            (= n 2))
        1
        (rec 3 1 (ash n -2)))))
