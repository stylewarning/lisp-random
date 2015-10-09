;;;; plaintext-format.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

(in-package #:hashlife)

;;; A parser for the plaintext .cells format.

(defun read-plaintext-file (filespec)
  (labels ((parse-line (line)
             (cond
               ;; Empty line... considered to be a line of all off
               ;; cells.
               ((zerop (length line)) nil)
               ;; Comment line... skip.
               ((char= #\! (char line 0)) ':skip)
               ;; Row spectification, with lax error conditions.
               ((or (char= #\. (char line 0))
                    (char= #\O (char line 0)))
                (loop :for c :across line
                      :when (char= #\. c)
                        :collect 0
                      :when (char= #\O c)
                        :collect 1))
               ;; Skip unrecognized line.
               (t ':skip))))
    (with-open-file (s filespec :direction ':input
                                :if-does-not-exist ':error)
      (let* ((parsed-lines (loop :for line := (read-line s nil nil nil)
                                   :then (read-line s nil nil nil)
                                 :while line
                                 :for parsed := (parse-line line)
                                 :unless (eq ':skip parsed)
                                   :collect (parse-line line)))
             (height (length parsed-lines))
             (width (loop :for line :in parsed-lines :maximize (length line)))
             (level (ceiling (log (max height width) 2)))
             (final-width (expt 2 level))
             (final-height final-width)
             (width-offset (floor (- final-width width) 2))
             (height-offset (floor (- final-height height) 2))
             (final-macrocell (empty-macrocell level)))
        (loop :for line :in parsed-lines
              :for y :from (- height-offset (/ final-height 2))
              :do (loop :for cell :in line
                        :for x :from (- width-offset (/ final-width 2))
                        :do (setf final-macrocell (macrocell-set-cell cell
                                                                      final-macrocell
                                                                      x
                                                                      y)))
              :finally (return final-macrocell))))))
