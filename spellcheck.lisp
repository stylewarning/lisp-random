;;;; spellcheck.lisp
;;;;
;;;; Copyright (c) 2018 Robert Smith

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun split (string)
    (loop :for beg := (position #\Newline string :test #'char/=)
            :then (position #\Newline string :start (1+ end) :test #'char/=)
          :for end := (and beg (position #\Newline string :start beg :test #'char=))
          :when beg :collect (subseq string beg end)
            :while end))
  
  (defun misspelled-words (string)
    "Identify the list of misspelled words in the string STRING."
    (split (with-input-from-string (*standard-input* string)
             (uiop:run-program '("aspell" "list")
                               :input t
                               :output '(:string :stripped t))))))

(defmacro spellchecked (string &key ignore)
  "Check the string STRING for misspelled words. Warn, at compile time, about possibly misspellt words."
  (check-type string string)
  (when (stringp ignore) (setf ignore (list ignore)))
  (dolist (word (misspelled-words string))
    (unless (member word ignore :test #'string=)
      (warn "You may have misspelled the word ~S." word)))
  string)
