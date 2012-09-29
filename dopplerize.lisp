;;;; dopplerize.lisp
;;;; Copyright (c) 2012 ROBERT SMITH

(defun dopplerize (string &key (growth-function 'identity))
  (labels ((spaces (n)
             (make-string (floor n) :initial-element #\Space)))
    (with-output-to-string (s)
      (loop :with length := (length string)
            :for c :across string
            :for i :from 1
            :do (if (= i length) ; last char?
                    (format s "~C" c)
                    (format s "~C~A" c (spaces (funcall growth-function i))))))))

(defun space-free-modified-dopplerize (string &key (growth-function 'identity))
  (dopplerize (remove #\Space string) :growth-function growth-function))

(defun triangle (n &optional (period 8))
  (let ((m (mod n period)))
    (if (> m (floor period 2))
        (- period m)
        m)))
