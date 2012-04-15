;;;; factorial.lisp
;;;; Copyright (c) 2012 Robert Smith

;;; Prime Numbers

(defun primes (limit)
  (if (< limit 2)
      nil
      (let* ((len (+ (floor limit 2)
                     (mod limit 2)
                     -1))
             (sieve (make-array (1+ len)
                                :element-type 'bit
                                :initial-element 1)))
        (loop :for i :below (floor (floor (sqrt limit)) 2)
              :unless (zerop (aref sieve i))
                :do (loop :for j
                            :from (+ 3 (* 2 i (+ 3 i))) :below len
                              :by (+ 3 (* 2 i))
                          :do (setf (aref sieve j) 0))
              :finally (return
                         (cons 2
                               (loop :for i :below len
                                     :unless (zerop (aref sieve i))
                                       :collect (+ 3 (* 2 i)))))))))