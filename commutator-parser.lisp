;;;; copyright (c) 2019 robert smith

(defvar *allow-empty-commutators* t "Allow [;x] or [x;] or [;].")

(defun parse (str)
  (with-input-from-string (s str)
    (read-seq s t)))

(defun ignore-char (s)
  (read-char s)
  (values))

(defun read-seq (s toplevel)
  (let ((seq (list ':seq)))
    (loop
      (case (peek-char nil s (not toplevel) ':eof)
        ((#\R #\U) (push (read-char s) seq))
        ((#\[) (push (read-comm s) seq))
        ((#\Space) (ignore-char s))
        ((:eof) (return (nreverse seq)))
        (otherwise (if toplevel (error "bad shrimp") (return (nreverse seq))))))))

(defun read-comm (s)
  (assert (eql #\[ (read-char s)))
  (let ((left (read-seq s nil))
        (kind (ecase (read-char s)
                ((#\,) :comm)
                ((#\;) :conj)))
        (right (read-seq s nil)))
    (assert (eql #\] (read-char s)))
    (unless *allow-empty-commutators*
      (assert (and (endp (rest left))
                   (endp (rest right)))))
    (list kind left right)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; test cases ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *good* '("R" "RUR" "[R;U]" "[[R;U];R]" "R[R;U]" "R[RR;U]" "R[RR[U;U];U]" "[R;UUR]"))
(defparameter *bad* '("RR;U]" "R[RU]" "[" "[R;R;"))
(defparameter *maybe* '("[;U]" "[RU;]" "[U;]"))

(defun test ()
  (labels ((safe-parse (x) (ignore-errors (parse x)))
           (all-good (x) (notany #'null (mapcar #'safe-parse x)))
           (all-bad (x) (every #'null (mapcar #'safe-parse x))))
    (assert (all-good *good*))
    (assert (all-bad *bad*))
    (let ((*allow-empty-commutators* t))
      (assert (all-good *maybe*)))
    (let ((*allow-empty-commutators* nil))
      (assert (all-bad *maybe*)))))
