;;;; vertical-banner.lisp
;;;; Copyright (c) 2013 Robert Smith

;;; CL-USER> (vertical-banner "HELLO")
;;;      H ELLO
;;;    H E LLO
;;;   HE L LO
;;;  HEL L O
;;; HELL O 

(defun print-spaces (n)
  (loop :repeat n
        :do (write-char #\Space)))

(defun write-word-with-spaces-at (pos word)
  (loop :for i :from 0
        :for c :across word
        :if (= i pos) :do 
          (write-char #\Space)
          (write-char c)
          (write-char #\Space)
        :else :do
          (write-char c)))

(defun vertical-banner (string)
  (let* ((len (length string)))
    (dotimes (i len)
      (print-spaces (- len i 1))
      (write-word-with-spaces-at i string)
      (terpri))))

;;; pjb's shorter version:
#+(or)
(defun vertical-banner (string)
  (loop
    with word = string
    with len = (length word)
    for i from 0
    for letter across word
    do (format t "~&~V<~>~A ~A ~A~%" (- len i) (subseq word 0 i) letter (subseq word (1+ i)))
    finally (return (values))))
