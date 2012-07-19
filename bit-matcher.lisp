
(declaim (inline ones))
(defun ones (n)
  (1- (ash 1 n)))

(defun decompose (sizes n)
  (loop :for len := (integer-length n) :then (- len size)
        :for size :in sizes
        :collect (ldb (byte size (- len size)) n)))

(defun generate-inline (pattern n &optional body)
  (let ((vars (mapcar #'first pattern))
        (vals (mapcar #'second pattern)))
    `(let ,(mapcar #'list vars (decompose vals n))
       ,@body)))

(defun generate-code (pattern n &optional body)
  (let ((reg (gensym))
        (len (gensym)))
    `(let* ((,reg ,n)
            (,len (integer-length ,reg)))
       ,(loop :for (var val) :in pattern
              :collect `(,var (prog1 (ash ,reg (- ,val ,len))
                                (setf ,reg (logand ,reg
                                                   (ones (- ,len ,val))))
                                (decf ,len ,val)))
                :into bindings
              :finally (return `(let* ,bindings
                                  ,@body))))))

(defun static-fields-p (fields)
  (every #'integerp (mapcar #'second fields)))

(defmacro with-bits ((&rest fields) n &body body)
  (if (and (integerp n)
           (static-fields-p fields))
      (generate-inline fields n body)
      (generate-code fields n body)))