;;;; string-equal-upto.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

(defun string-equal-upto (a b insignificantp)
  (let ((a-length (length a))
        (b-length (length b)))
    (labels ((rest-insignificantp (s ptr len)
               (loop :for s-ptr :from ptr :below len
                     :always (funcall insignificantp (char s s-ptr))))
             (next-significant-char (s ptr len)
               (loop :for s-ptr :from ptr :below len
                     :for char := (char s s-ptr)
                     :unless (funcall insignificantp char)
                       :do (return (values char (1+ s-ptr)))
                     :finally (return (values nil len))))
             (inspect-string (a-ptr b-ptr)
               (multiple-value-bind (a-char new-a-ptr)
                   (next-significant-char a a-ptr a-length)
                 (if (null a-char)
                     (rest-insignificantp b b-ptr b-length)
                     (multiple-value-bind (b-char new-b-ptr)
                         (next-significant-char b b-ptr b-length)
                       (if (or (null b-char)
                               (char/= a-char b-char))
                           nil
                           (inspect-string new-a-ptr new-b-ptr)))))))
      (inspect-string 0 0))))

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
    (assert (not (string-equal-upto-whitespace s2 s4)))))
