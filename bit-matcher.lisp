;;; Things that can affect optimization...
;;;
;;; * Constant value (CV)
;;; * Constant bindings (CB)
;;; * Back references (BR)
;;; * Kleene star (KS)
;;;
;;; CV + CB ===> complete compile-time destructuring
;;; CB      ===> LSB -> MSB
;;; BR      ===> MSB -> LSB


;;;; Some utilities

(declaim (inline ones))
(defun ones (n)
  (1- (ash 1 n)))

(defun static-fields-p (fields)
  (every #'integerp (mapcar #'second fields)))


;;;; Code generators

(defun decompose (sizes n)
  (loop :for len := (integer-length n) :then (- len size)
        :for size :in sizes
        :collect (ldb (byte size (- len size)) n)))

;;; This is a complete compile-time destructuring
(defun generate-inline (pattern n &optional body)
  (let ((vars (mapcar #'first pattern))
        (vals (mapcar #'second pattern)))
    `(let ,(mapcar #'list vars (decompose vals n))
       ,@body)))

;;; This does an MSB -> LSB destructuring and assumes
;;;   * Non-CV
;;;   * Non-CB
;;;   * Back references
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


;;;; The Real Deal (tm)

(defmacro with-bits ((&rest fields) n &body body)
  (if (and (integerp n)
           (static-fields-p fields))
      (generate-inline fields n body)
      (generate-code fields n body)))