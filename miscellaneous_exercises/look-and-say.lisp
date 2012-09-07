;;;; look-and-say.lisp
;;;; Copyright (c) 2012 Robert Smith

;;;; Implementation of the look-and-say sequence.

(defun split-head (string)
  "Find the longest uniform substring at the beginning of STRING and
return it and its tail. For example, '111123' --> '1111' and '23'."
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
  "Partition a string into its component uniform substrings. For
example, 111122333455 -> (1111 22 333 4 55)."
  (if (null string)
      nil
      (loop :for (x . xs) := (split-head string)
            :then (split-head xs)
            :while xs
            :collect x)))

(defun look-say-transform (string)
  "Transform a uniform string into it's look-say pattern. For example,
11 -> 21, 111 -> 31."
  (assert (not (string= "" string)))
  (format nil "~D~C"
          (length string)
          (elt string 0)))

(defun next-look-say (string)
  "Compute the next look-say element after STRING."
  (with-output-to-string (s)
    (mapc (lambda (string)
            (princ string s))
          (mapcar #'look-say-transform (partition string)))))

(defun look-say (n &optional (seed "1"))
  "Compute the Nth look-say after SEED."
  (loop :repeat (1+ n)
        :for ls := seed :then (next-look-say ls)
        :finally (return ls)))
