(defun rational-split (r)
  (multiple-value-list (truncate r)))

(defun rational->cf (r)
  (loop
     :for proper := (rational-split r)
     :collect (first proper) :into cf
     :until (zerop (second proper))
     :do (setf r (/ (second proper)))
     :finally (return cf)))

(defun pi-cf (file &optional (precision nil))
  (let* ((pi-digits-string
          (subseq (with-open-file (s file)
                    (read-line s))
                  0 precision))
         (pi-prec (or precision (length pi-digits-string)))
         (pi-digits (/ (parse-integer pi-digits-string)
                       (expt 10 (1- pi-prec)))))
    (rational->cf pi-digits)))