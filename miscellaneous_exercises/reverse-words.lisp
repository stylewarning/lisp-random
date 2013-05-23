;;;; reverse-words.lisp
;;;; Copyright (c) 2013 Robert Smith

;;;; Exercise: Reverse the letters of words, delimited by spaces, in a
;;;; string in place.
;;;;
;;;; Example: "Hello my name is:    Bob." ==> "olleH ym eman :si    .boB"

;; We can technically have a single loop variable, counting up to half
;; the substring length. But I think this is a clearer way.
(defun reverse-substring! (string start end)
  "Reverse the substring of characters in the string STRING from the
index START to before the index END."
  (loop :for i :from start    :to (1- end)
        :for j :from (1- end) :downto start
        :while (< i j)
        :do (rotatef (aref string i)
                     (aref string j))))

(defun reverse-words! (string)
  "Reverse the individual words in the string STRING in place."
  (let ((len (length string))
        (start 0))
    (dotimes (i (1+ len) string)
      (when (or (= len i) (char= #\Space (aref string i)))
        (reverse-substring! string start i)
        (setf start (1+ i))))))

(defun reverse-words (string)
  "Reverse the words in the string STRING."
  (reverse-words! (copy-seq string)))


;; Note that 0 bytes were allocated during the REVERSE-WORDS
;; computation.
;;
;; CL-USER> (let ((s (copy-seq "Hello my 1 name is quad")))
;;            (time (reverse-words! s)))
;;
;; Timing the evaluation of (REVERSE-WORDS S)
;;
;; User time             =        0.000
;; System time           =        0.000
;; Elapsed time          =        0.000
;; Allocation   = 0 bytes
;; 0 Page faults
;; "olleH ym 1 eman si dauq"


;;;; Exercise: Reverse the order of words in a sentence.
;;;;
;;;; Example: "Hello my name is:    Bob." ==> "Bob.    is: name my Hello"

(defun reverse-sentence! (string)
  "Reverse the order of the words in the sentence STRING in place."
  (reverse-words! (nreverse string)))

(defun reverse-sentence (string)
  (reverse-sentence! (copy-seq string)))
