;;;; terminal-marquee.lisp
;;;; Copyright (c) 2013 Robert Smith

(defun escape (format-string &rest args)
  (write-char #\Esc)
  (write-char #\[)
  (apply #'format t format-string args))

(defun locate (line col)
  (escape "~D;~DH" line col))

(defun clear ()
  (escape "2J"))


(defun main ()
  (loop :for col := 0 :then (mod (1+ col) (- 80 23))
        :do (progn
              (clear)
              (locate 10 col)
              (write-string "hello lisp this is dog!")
              (finish-output)
              (sleep 1/20))))



;;; Execute MAIN.

(main)
