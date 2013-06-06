;;;; morse-code.lisp
;;;; Copyright (c) 2013 Robert Smith

(defun acceptable-morse-character-p (char)
  "Can the character CHAR be translated into Morse code?"
  (or (char= char #\Space)
      (alphanumericp char)))

(defun char-to-morse-code (char)
  "Convert a character CHAR to Morse code. Return NIL if the character
could not be translated."
  (let ((+table+ '((#\a . ".-")    (#\b . "-...")  (#\c . "-.-.")  (#\d . "-..")
                   (#\e . ".")     (#\f . "..-.")  (#\g . "--.")   (#\h . "....")
                   (#\i . "..")    (#\j . ".---")  (#\k . "-.-")   (#\l . ".-..")
                   (#\m . "--")    (#\n . "-.")    (#\o . "---")   (#\p . ".--.")
                   (#\q . "--.-")  (#\r . ".-.")   (#\s . "...")   (#\t . "-")
                   (#\u . "..-")   (#\v . "...-")  (#\w . ".--")   (#\x . "-..-")
                   (#\y . "-.--")  (#\z . "--..")
                   (#\1 . ".----") (#\2 . "..---") (#\3 . "...--") (#\4 . "....-")
                   (#\5 . ".....") (#\6 . "-....") (#\7 . "--...") (#\8 . "---..")
                   (#\9 . "----.") (#\0 . "-----")
                   (#\Space "/"))))
    (cdr (assoc char +table+ :test #'char-equal))))

(defun morse-code (string &key remove-invalid-chars)
  "Convert the string STRING to Morse code. If REMOVE-INVALID-CHARS is
T, then invalid characters are skipped. Otherwise, they are replaced
with a question mark."
  (let ((string (if remove-invalid-chars
                    (remove-if-not #'acceptable-morse-character-p string)
                    string)))
    (string-trim
     " "
     (with-output-to-string (stream)
       (loop :with space-flag := nil
             :for c :across string
             :do (if (char= #\Space c)
                     (unless space-flag
                       (princ "/ " stream)
                       (setf space-flag t))
                     (let ((morse (char-to-morse-code c)))
                       (setf space-flag nil)
                       (princ (or morse "?") stream)
                       (princ " " stream))))))))
