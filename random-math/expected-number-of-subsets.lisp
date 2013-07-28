;;;; expected-number-of-subsets.lisp
;;;; Copyright (c) 2013 Robert Smith


(defun run-simulation (n)
  (let* ((max (expt 2 n))
         (full (1- max)))
    (loop :for i :from 0
          :for r := 0 :then (logior r (random max))
          ;; :do (format t "~B~%" r)
          :until (= full r)
          :finally (return i))))

(defun run-sims (max &optional (trials 10000))
  (flet ((run-sim (n)
           (loop :repeat trials
                 :sum (run-simulation n) :into s
                 :finally (return (float (/ s trials))))))
    (loop :for i :from 2 :to max
          :do (format t "~D: ~D~%" i (run-sim i)))))

;;; Algebraist says E(n) is f(n)=1+sum_i=0^n (n choose i) 2^{-n} f(i)
