;;;; copyright (c) 2019 robert smith

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
    (list kind left right)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; test cases ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *good* '("R" "RUR" "[R;U]" "[[R;U];R]" "R[R;U]" "R[RR;U]" "R[RR[U;U];U]" "[R;UUR]" "[;U]" "[RU;]" "[U;]"))
(defparameter *bad* '("RR;U]" "R[RU]" "[" "[R;R;"))

(defun test ()
  (flet ((safe-parse (x) (ignore-errors (parse x))))
    (assert (notany #'null (mapcar #'safe-parse *good*)))
    (assert (every #'null (mapcar #'safe-parse *bad*)))))
