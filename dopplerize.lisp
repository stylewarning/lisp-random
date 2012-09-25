;;;; dopplerize.lisp
;;;; Copyright (c) 2012 ROBERT SMITH

(defun dopplerize (string &key (growth-function 'identity))
  (labels ((spaces (n)
             (make-string (floor n) :initial-element #\Space)))
    (loop :with length := (length string)
          :for c :across string
          :for i :from 1
          :do (if (= i length) ; last char?
                  (format t "~C~%" c)
                  (format t "~C~A" c (spaces (funcall growth-function i)))))))

