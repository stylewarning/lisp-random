(defun alist-to-hash-table (alist)
  (loop :with ht := (make-hash-table)
        :for (k v) :in alist
        :do (setf (gethash k ht) (eval v))
        :finally (return ht)))

(set-macro-character
 #\{
 (lambda (stream char)
   (declare (ignore char))
   (let ((lst (read-delimited-list #\} stream t)))
     (alist-to-hash-table lst))))


;;; CL-USER> {(:x 1) (:y 2) (:z (+ 1 1))}
;;; #<HASH-TABLE :TEST EQL :COUNT 3 {128A8141}>
;;; CL-USER> '{(:x 1) (:y 2) (:z (+ 1 1))}
;;; #<HASH-TABLE :TEST EQL :COUNT 3 {128B0641}>
;;; CL-USER> (gethash :z {(:x 1) (:y 2) (:z (+ 1 1))})
;;; 2
;;; T

;;; This does not implement the things the spec requires.
(defmethod print-object ((x hash-table) stream)
  (princ #\{ stream)
  (princ #\Space stream)
  (maphash (lambda (k v) (format stream "(~S ~S) " k v)) x)
  (princ #\} stream))