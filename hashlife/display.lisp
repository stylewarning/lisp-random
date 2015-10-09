;;;; display.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

(in-package #:hashlife)

(defun show (mc &optional (stream *standard-output*))
  "Pretty print a macrocell MC to the stream STREAM, which is *STANDARD-OUTPUT* by default."
  (let* ((width (macrocell-width mc))
         (width/2 (/ width 2)))
    ;; Draw top border.
    (write-char #\+ stream)
    (loop :repeat width :do (write-char #\- stream))
    (write-char #\+ stream)
    (terpri stream)
    
    ;; Draw pattern.
    (loop :for y :from (- width/2) :below width/2 :do
      (write-char #\| stream)
      (loop :for x :from (- width/2) :below width/2 :do
        (if (= 1 (macrocell-cell mc x y))
            (write-char #\* stream)
            (write-char #\Space stream)))
      (write-char #\| stream)
      (terpri stream))
    
    ;; Draw top border.
    (write-char #\+ stream)
    (loop :repeat width :do (write-char #\- stream))
    (write-char #\+ stream)
    (terpri stream)))
