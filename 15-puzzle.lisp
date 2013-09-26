;;;; 15-puzzle.lisp
;;;; Copyright (c) 2013 Robert Smith

;;; The board is stored in a 16 element of integers whose values
;;; correspond to the following solved positions.
;;;
;;; +----+----+----+----+
;;; |  0 |  1 |  2 |  3 |
;;; +----+----+----+----+
;;; |  4 |  5 |  6 |  7 |
;;; +----+----+----+----+
;;; |  8 |  9 | 10 | 11 |
;;; +----+----+----+----+
;;; | 12 | 13 | 14 | 15 |
;;; +----+----+----+----+
;;;
;;; The number 15 is the empty space.

(defconstant +space+ 15)

(defun make-puzzle ()
  "Make a new, solved 15-puzzle."
  (let ((puzzle (make-array 16)))
    (dotimes (i 16 puzzle)
      (setf (aref puzzle i) i))))

;;; We will want to be able to print the puzzle.

(defun print-puzzle (puzzle)
  "Print a puzzle prettily."
  (dotimes (pos 16)
    (when (zerop (mod pos 4))
      (format t "~%+----+----+----+----+~%|"))
    (let ((piece (aref puzzle pos)))
      (if (= +space+ piece)
          (write-string "    |")
          (format t " ~2,' D |" (1+ piece)))))
  (format t "~%+----+----+----+----+~%")
  nil)

;;; Now the four principal moves.

(defun slide-down (puzzle)
  "Slide a piece down into the space, if possible."
  (let ((space-position (position +space+ puzzle :test #'=)))
    (unless (<= 0 space-position 3)
      (rotatef (aref puzzle space-position)
               (aref puzzle (- space-position 4)))))
  puzzle)

(defun slide-up (puzzle)
  "Slide a piece up into the space, if possible."
  (let ((space-position (position +space+ puzzle :test #'=)))
    (unless (<= 12 space-position 15)
      (rotatef (aref puzzle space-position)
               (aref puzzle (+ space-position 4)))))
  puzzle)

(defun slide-left (puzzle)
  "Slide a piece left into the space, if possible."
  (let ((space-position (position +space+ puzzle :test #'=)))
    (unless (= 3 (mod space-position 4))
      (rotatef (aref puzzle space-position)
               (aref puzzle (1+ space-position)))))
  puzzle)

(defun slide-right (puzzle)
  "Slide a piece right into the space, if possible."
  (let ((space-position (position +space+ puzzle :test #'=)))
    (unless (zerop (mod space-position 4))
      (rotatef (aref puzzle space-position)
               (aref puzzle (1- space-position)))))
  puzzle)

;;; Now a way to scramble.

(defun scramble (puzzle)
  "Scramble the puzzle randomly."
  (dotimes (i 100)
    (case (random 4)
      ((0) (slide-left puzzle))
      ((1) (slide-right puzzle))
      ((2) (slide-up puzzle))
      ((3) (slide-down puzzle))))
  puzzle)

;;; Lastly, an interactive shell.

(defun prompt (string)
  (write-string string)
  (write-string " ")
  (finish-output)
  (read-line *query-io* nil nil nil))

(defun play ()
  "Play with a 15-puzzle."
  (let ((puzzle (make-puzzle)))
    (loop :named game
          :initially (print-puzzle puzzle)
          :for input := (prompt "Command?")
          :do (progn
                (when (zerop (length input))
                  (return-from game))
                (case (char-upcase (aref input 0))
                  ((#\U) (slide-up puzzle)
                         (print-puzzle puzzle))
                  ((#\D) (slide-down puzzle)
                         (print-puzzle puzzle))
                  ((#\L) (slide-left puzzle)
                         (print-puzzle puzzle))
                  ((#\R) (slide-right puzzle)
                         (print-puzzle puzzle))
                  ((#\S) (scramble puzzle)
                         (print-puzzle puzzle))
                  ((#\Q #\E) (return-from game)))))))
