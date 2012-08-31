;;;; one-offs.lisp
;;;; Copyright (c) 2012 Robert Smith


;;;;;;;;;;;;;;;;;;;;;; Incremental Fisher-Yates ;;;;;;;;;;;;;;;;;;;;;;

(defun make-fisher-yates-generator (max-value)
  "Create a generator which will generate the values between 0 and
MAX-VALUE - 1 exactly once in shuffled order."
  (let ((choices (loop :for i :below max-value :collect i)))
    (lambda ()
      (unless (null choices)
        (let ((random-index (random (length choices))))
          (prog1 (nth random-index choices)
            ;; Remove the Nth element from our choices.
            (setf choices (delete-if (constantly t) choices
                                     :start random-index
                                     :count 1))))))))


;;;;;;;;;;;;;;;;;;;;;; [0, N) -> [0, M) RNG ;;;;;;;;;;;;;;;;;;;;;;;

(defun test-rng (rng n &optional (trials 1000))
  "Generate an experimental distribution of RNG from 0 to N-1 over
TRIALS number of trials."
  (let ((pdf (make-array n :initial-element 0)))
    (dotimes (i trials)
      (incf (aref pdf (funcall rng n))))

    (map 'list (lambda (x) (float (/ x trials))) pdf)))

(defparameter *random-upper-bound* 8) ; N

(defun fixed-random ()
  "Generate a random number in a fixed range
[0, *RANDOM-UPPER-BOUND*)."
  (random *random-upper-bound*))


;;; In this example, a random number generator would have bias in the
;;; first three numbers since the remainder of the upper bound and 5 is
;;; 3 (recall, the upper bound isn't actually a possible random
;;; number, it is one more than the maximum random number). But, by
;;; identifying the biased range, we can skip it to produce truly
;;; unbiased numbers.
;;;
;;;CL-USER> (test-rng #'random-from-fixed 5 1000000)
;;;(0.199945 0.200182 0.200459 0.19977 0.199644)
(defun random-from-fixed (m)
  "Generate a random number between 0 and M from the random number
  generator FIXED-RANDOM."
  (assert (<= m *random-upper-bound*)
          (m)
          "M must be at most ~D."
          *random-upper-bound*)
  
  (let ((valid-range (- *random-upper-bound*
                        (mod *random-upper-bound* m))))
    (loop :for r := (fixed-random) :then (fixed-random)
          :until (< r valid-range)
          :finally (return (mod r m)))))

;;; We can observe the severe bias in a naive implementation of random
;;; range reduction here:
;;;
;;; CL-USER> (test-rng #'biased-random-from-fixed 5 1000000)
;;; (0.249849 0.249758 0.250589 0.124996 0.124808)
(defun biased-random-from-fixed (m)
  "Generate a biased random number between 0 and M from the random
number generator FIXED-RANDOM."
  (assert (<= m *random-upper-bound*)
          (m)
          "M must be at most ~D."
          *random-upper-bound*)
  
  (mod (fixed-random) m))