;;;; compress-spaces.lisp
;;;; Copyright (c) 2013 Robert Smith

(defun compress (s &key (max-length 2)
                        (char-to-compress #\Space))
  "Compress all runs of CHAR-TO-COMPRESS to sequences of at most
length MAX-LENGTH."
  (let ((current-write 0)
        (run-length    0))
    (dotimes (current-read (length s) (subseq s 0 current-write))
      (let ((current-char (char s current-read)))
        (if (char= current-char char-to-compress)
            (unless (> (incf run-length) max-length)
              (setf (char s current-write) current-char)
              (incf current-write))
            (progn
              (setf run-length 0)
              (setf (char s current-write) current-char)
              (incf current-write)))))))
