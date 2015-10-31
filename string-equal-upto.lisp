;;;; string-equal-upto.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

(deftype string-length ()
  `(integer 0 ,array-total-size-limit))

(deftype string-index ()
  `(integer 0 ,(1- array-total-size-limit)))

(defun string-equal-upto (a b insignificantp)
  "Given two SIMPLE-STRING values A and B, and a unary function INSIGNIFICANTP of type

    (FUNCTION (CHARACTER) BOOLEAN),

compute whether they are equal, ignoring characters satisfying INSIGNIFICANTP."
  (declare (optimize speed (safety 0) (debug 0) (space 0))
           (type simple-string a b)
           (type (function (character) boolean) insignificantp))
  (let ((a-length (length a))
        (b-length (length b)))
    (declare (type string-length a-length b-length))
    (labels ((rest-insignificantp (s ptr len)
               "Starting at and including the character at PTR, are the rest of the characters in the string S of length LEN insignificant?"
               (loop :for s-ptr :of-type string-index :from ptr :below len
                     :always (funcall insignificantp (schar s s-ptr))))
             (next-significant-char (s ptr len)
               "Starting at and including the character at PTR, what and where is the next significant character in the string S of length LEN? Return two values: the character and its index. If no such character exists, then NIL and the length of the string will be returned."
               (loop :for s-ptr :of-type string-index :from ptr :below len
                     :for char :of-type character := (schar s s-ptr)
                     :unless (funcall insignificantp char)
                       :do (return (values char s-ptr))
                     :finally (return (values nil len))))
             (compare-starting-at (a-ptr b-ptr)
               "Compare the strings A and B starting at the pointers (non-negative integers) A-PTR and B-PTR within the strings respectively."
               (declare (type string-index a-ptr b-ptr))
               (multiple-value-bind (a-char a-char-ptr)
                   (next-significant-char a a-ptr a-length)
                 (declare (type (or null character) a-char)
                          (type string-length a-char-ptr))
                 (if (null a-char)
                     (rest-insignificantp b b-ptr b-length)
                     (multiple-value-bind (b-char b-char-ptr)
                         (next-significant-char b b-ptr b-length)
                       (declare (type (or null character) b-char)
                                (type string-length b-char-ptr))
                       (if (or (null b-char)
                               (char/= a-char b-char))
                           nil
                           (compare-starting-at (1+ a-char-ptr) (1+ b-char-ptr))))))))
      (declare (inline next-significant-char rest-insignificantp))
      (compare-starting-at 0 0))))

(defun string-equal-upto-whitespace (a b)
  (string-equal-upto a b (lambda (c)
                           (member c
                                   '(#\Space #\Newline #\Backspace #\Tab 
                                     #\Linefeed #\Page #\Return #\Rubout)
                                   :test #'char=))))

(defun test ()
  (let ((s1 " xxx")
        (s2 (format nil "  x~%x  x  "))
        (s3 "xxxx")
        (s4 "")
        (s5 (format nil "  ~%  ")))
    (assert (string-equal-upto-whitespace s1 s2))
    (assert (string-equal-upto-whitespace s1 s1))
    (assert (string-equal-upto-whitespace s2 s2))
    (assert (string-equal-upto-whitespace s3 s3))
    (assert (string-equal-upto-whitespace s4 s4))
    (assert (string-equal-upto-whitespace s4 s5))

    (assert (not (string-equal-upto-whitespace s1 s3)))
    (assert (not (string-equal-upto-whitespace s2 s3)))
    (assert (not (string-equal-upto-whitespace s2 s4)))
    (write-line "success")
    nil))
