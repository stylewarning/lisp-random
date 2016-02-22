;;;; deadfish.lisp
;;;;
;;;; Copyright (c) 2016 Robert Smith

;;;; Puzzle is here: http://codegolf.stackexchange.com/questions/40124/short-deadfish-numbers/

(load "enumerate-grammar")

(ql:quickload :global-vars)

;;; NOTE: This could be done more efficiently by enumerating base-4
;;; non-negative integers.
(global-vars:define-global-var **deadfish-grammar**
    (with-non-terminals (expr cmd)
      (make-grammar expr
        :expr (alternates cmd (list cmd expr))
        :cmd (alternates (terminal "i")
                         (terminal "s")
                         (terminal "d")
                         (terminal "o"))))
  "Grammar defining the Deadfish language.")

(defun execute-sentence (sentence)
  "Given a sentence SENTENCE from the Deadfish language, execute it to produce an integer result."
  (labels ((normalize-register (register)
             ;; Required according to http://esolangs.org/wiki/Deadfish
             (if (or (= -1 register)
                     (= 256 register))
                 0
                 register))
           (run (sentence register history)
             (if (null sentence)
                 (if (zerop (length history))
                     0
                     (parse-integer (format nil "~{~D~}" (nreverse history))
                                    :junk-allowed nil))
                 (let* ((cmd (first sentence))
                        (rest (rest sentence)))
                   (adt:match sym cmd
                     ((terminal x)
                      (cond
                        ((string= "i" x)
                         (run rest (normalize-register (1+ register)) history))
                        ((string= "s" x)
                         (run rest (normalize-register (* register register)) history))
                        ((string= "d" x)
                         (run rest (normalize-register (1- register)) history))
                        ((string= "o" x)
                         (run rest register (cons register history)))))
                     ((non-terminal _) (error "Invalid state: Found non-terminal ~A." cmd)))))))
    (run sentence 0 nil)))

;;; Challenge: Find all of the shortest Deadfish sentences which
;;; compute 0 to 255 inclusive.

(defun sentence-string (sentence)
  (flet ((strip (thing)
           (adt:match sym thing
             ((terminal x) x)
             ((non-terminal _) ""))))
    (apply #'concatenate 'string (mapcar #'strip sentence))))

(defun challenge ()
  (let ((sentences (make-array 256 :initial-element nil))
        (numbers-left 256))
    (flet ((fish (sentence)
             (let ((value (execute-sentence sentence)))
               (cond
                 ;; Value is in range.
                 ((not (<= 0 value 255)) nil)
                 ;; Value has already been found.
                 ((aref sentences value) nil)
                 ;; We got a new one, boys.
                 (t
                  (format t "~D/~D: ~A~%" value numbers-left (sentence-string sentence))
                  (setf (aref sentences value) sentence)
                  (decf numbers-left)))
               (when (zerop numbers-left)
                 (return-from challenge (map 'vector #'sentence-string sentences))))))
      (map-sentences #'fish **deadfish-grammar**))))
