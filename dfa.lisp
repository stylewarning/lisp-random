;;;; dfa.lisp
;;;; Copyright (c) 2013 Robert Smith


;;;; Special edges and utilities

(defconstant nul (code-char 0)
  "No transition.")

(defconstant eps (code-char 1)
  "Epsilon transition.")

(defun nul? (char)
  (char= char nul))

(defun eps? (char)
  (char= char eps))

(defun array-from-list (list-rep)
  (let ((rows (length list-rep))
        (cols (length (first list-rep))))
    (make-array (list rows cols)
                :initial-contents list-rep)))

(defun explode (string)
  (coerce string 'list))

;;;; DFA Representation

(defstruct dfa
  state-count
  alphabet
  start-state
  accepting-states
  transition-table)

(defun dfa-of-transition-table (table accepting-states)
  (make-dfa
   :state-count (length (rest table))
   :alphabet (coerce (first table) 'vector)
   :start-state 0                       ; Do we want this as the DEFAULT?
   :accepting-states accepting-states
   :transition-table (array-from-list (rest table))))

(defun transition (dfa from-state edge)
  (let ((idx (position edge (dfa-alphabet dfa))))
    (and idx
         (aref (dfa-transition-table dfa) from-state idx))))

(defun accepting-state-p (dfa state)
  (and (find state (dfa-accepting-states dfa))
       t))

(defun match-string (dfa string)
  (labels ((step (state chars)
             (cond
               ((null state) nil)
               ((null chars) (accepting-state-p dfa state))
               (t (step (transition dfa state (car chars))
                        (cdr chars))))))
    (step (dfa-start-state dfa)
          (explode string))))
