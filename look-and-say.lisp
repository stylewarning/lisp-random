;;;; look-and-say.lisp
;;;; Copyright (c) 2012 Robert Smith

;;;; Implementation of the look-and-say sequence.

(defun split-head (string)
  (if (string= string "")
      (cons nil nil)
      (let ((next (position (elt string 0)
                            string
                            :test (lambda (a b) (char/= a b)))))
        (if (null next)
            (cons string "")
            (cons (subseq string 0 next)
                  (subseq string next))))))

(defun partition (string)
  (if (null string)
      nil
      (loop :for (x . xs) := (split-head string)
            :then (split-head xs)
            :while xs
            :collect x)))

(defun look-say-transform (string)
  (assert (not (string= "" string)))
  (format nil "~D~C"
          (length string)
          (elt string 0)))

(defun next-look-say (string)
  (with-output-to-string (s)
    (mapc (lambda (string)
            (princ string s))
          (mapcar #'look-say-transform (partition string)))))

(defun look-say (n &optional (seed "1"))
  (loop :repeat (1+ n)
        :for ls := seed :then (next-look-say ls)
        :finally (return ls)))