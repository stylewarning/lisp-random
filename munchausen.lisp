(deftype non-negative-fixnum ()
  `(and fixnum unsigned-byte))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-powers ()
    (let ((powers (make-array 10 :element-type 'non-negative-fixnum
                                 :initial-element 0)))
      (dotimes (i 10 powers)
        (unless (zerop i)               ; Use non-standard 0^0 = 0
          (setf (aref powers i) (expt i i)))))))

(defmacro do-digits ((d n) &body body)
  (let ((reduced-n (gensym "REDUCED-N"))
        (q (gensym "Q-")))
    `(loop :with ,reduced-n :of-type non-negative-fixnum := ,n
           :do (multiple-value-bind (,q ,d) (floor ,reduced-n 10)
                 (declare (type non-negative-fixnum ,q)
                          (type (integer 0 9) ,d))
                 (setf ,reduced-n ,q)
                 ,@body)
           :until (zerop ,reduced-n))))

(declaim (inline munchausenp))
(declaim (ftype (function (non-negative-fixnum) boolean) munchausenp))
(defun munchausenp (n)
  (declare (type non-negative-fixnum n)
           (optimize speed (safety 0) (debug 0) (space 0)))
  (let ((powers (load-time-value (make-powers) t)))
    (declare (type (simple-array non-negative-fixnum (10)) powers))
    (do-digits (d n)
      (let ((p (aref powers d)))
        (declare (type non-negative-fixnum p))
        (if (> p n)
            (return-from munchausenp nil)
            (decf n p)))))
  (zerop n))

(defun find-munchausen (limit)
  (declare (type non-negative-fixnum limit))
  (format t "Computing all Munchausen numbers below ~R.~%" limit)
  (loop :for i :of-type non-negative-fixnum :below limit
        :when (munchausenp i)
          :collect i))
