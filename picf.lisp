;; Copyright (c) 2011 Robert Smith
;; Inspired by Neil Bickford's <http://neilbickford.com/picf.htm>

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
          (subseq (with-open-file (s file) (read-line s))
                  0 precision))
         (pi-digits (/ (parse-integer pi-digits-string)
                       (expt 10 (1- (or precision
                                        (length pi-digits-string)))))))
    (rational->cf pi-digits)))