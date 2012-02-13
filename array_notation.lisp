;; Dispatch on the `[' character to AREF-BRACKET
(set-macro-character
 #\[
 (lambda (stream char)
   (declare (ignore char))
   (let* ((lst (read-delimited-list #\] stream t)))
     (aref-bracket lst))))

(defun aref-bracket (args)
  (let ((arr (car args))
        (index (cdr args)))
    (if (null index)
        arr
        `(aref ,arr ,@index))))

;; Let `]' be equivalent to `)'
(set-macro-character
 #\]
 (get-macro-character #\)))

;; CL-USER> (defparameter x (make-array 5 :initial-contents '(zero one two three four)))
;; X
;; CL-USER> (defparameter y (make-array '(3 3) :initial-contents '((aa ab ac)
;;                                                                 (ba bb bc)
;;                                                                 (ca cb cc))))
;; Y
;; CL-USER> [x 0]
;; ZERO
;; CL-USER> [x 4]
;; FOUR
;; CL-USER> [y 0 2]
;; AC
;; CL-USER> [y 1 1]
;; BB

