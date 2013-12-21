;;;; expected-partitions.lisp
;;;; Copyright (c) 2013 Robert Smith

;; TODO: remember what I was trying to accomplish here.

(ql:quickload :dancing-links)

(in-package :dancing-links)

(defun run-simulation (size)
  (let ((subsets nil)
        (maximum (expt 2 size)))
    (labels ((add-subset ()
               (loop :with r := (random maximum)
                     :for i :below size
                     :when (logbitp i r)
                       :collect i :into subset
                     :finally (push subset subsets)))
             (compute-cover ()
               (let ((*standard-output* (make-broadcast-stream)))
                 (exact-cover size subsets))))
      
      (loop :for i :from 1
            :for c := (compute-cover) :then (compute-cover)
            :while (zerop c)
            :do (add-subset)
            :finally (return (values i subsets))))))

(defun run-simulations (max &optional (trials 100))
  (flet ((run-sim (n)
           (loop :repeat trials
                 :sum (run-simulation n) :into s
                 :finally (return (list s (float (/ s trials)))))))
    (loop :for i :from 2 :to max
          :collect (list* i (run-sim i)))))
