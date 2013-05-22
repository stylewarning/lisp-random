;;;; reverse-words.lisp
;;;; Copyright (c) 2013 Robert Smith

;;;; Given a string such as "abc def", reverse the words, delimited by
;;;; spaces, in the string in place.

(defun reverse-substring! (string start end)
  "Reverse the substring of characters in the string STRING from the
index START to before the index END."
  (loop :for i :from start    :to (1- end)
        :for j :from (1- end) :downto start
        :while (< i j)
        :do (rotatef (aref string i)
                     (aref string j))))

(defun reverse-words (string)
  "Reverse the words in the string STRING in place."
  (let ((len (length string)))
    (labels ((reverse-next-word (word-start)
               (if (>= word-start len)
                   string
                   (let ((word-end (or (position #\Space string :start word-start
                                                                :test #'char=)
                                       len)))
                     (reverse-substring! string word-start word-end)
                     (reverse-next-word (1+ word-end))))))
      (reverse-next-word 0))))

;; Note that 0 bytes were allocated during the REVERSE-WORDS
;; computation.
;;
;; CL-USER> (let ((s (copy-seq "Hello my 1 name is quad")))
;;            (time (reverse-words s)))
;;
;; Timing the evaluation of (REVERSE-WORDS S)
;;
;; User time             =        0.000
;; System time           =        0.000
;; Elapsed time          =        0.000
;; Allocation   = 0 bytes
;; 0 Page faults
;; "olleH ym 1 eman si dauq"
