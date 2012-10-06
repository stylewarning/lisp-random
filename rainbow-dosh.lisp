;;;; rainbow-dosh.lisp
;;;; Copyright (c) 2012 Robert Smith

;;; You are given 3 stacks of coins. The only operation you are
;;; permitted to perform is doubling one stack by taking that many
;;; from another. Prove (or devise an algorithm which you can show is
;;; correct) that shows you can always empty one of the stacks.

;;; Representation

(deftype move ()
  `(cons (integer 0 2) (integer 0 2)))

(defun make-move (a b)
  "Make a new move."
  (the move (cons a b)))

(defun from-stack (move)
  "Get the number of the stack to subtract from."
  (declare (type move move))
  (car move))

(defun to-stack (move)
  "Get the number of the stack to add to."
  (declare (type move move))
  (cdr move))

(deftype state ()
  `(simple-array fixnum (3)))

(defun normalize-state! (state)
  "Normalize the state STATE."
  (declare (type state state))
  (sort state #'<))

(defun make-state (a b c)
  "Make new state with piles of size A, B, and C. The piles will be
normalized."
  (normalize-state! (make-array 3 :element-type 'fixnum
                                  :initial-contents (list a b c))))

(defun copy-state (state)
  "Make a copy of STATE."
  (declare (type state state))
  (copy-seq state))


;;; Operations

(defun valid-move-p (state move)
  "Is it valid to move coins from the FROM-STACK to the TO-STACK?"
  (declare (type state state)
           (type move move))
  (<= (aref state (to-stack move))
      (aref state (from-stack move))))

(defun do-move (state move)
  "Move the coins from the stack numbered FROM-STACK to the stack
  numbered TO-STACK."
  (declare (type state state)
           (type move move))
  (let* ((from-stack (from-stack move))
         (to-stack   (to-stack move))
         (to        (aref state to-stack))
         (new-state (copy-state state)))
    (decf (aref new-state from-stack) to)
    (incf (aref new-state to-stack)   to)
    (normalize-state! new-state)))


;;; Solution

(defun solvedp (state)
  "Check if STATE is solved."
  (declare (type state state))
  (some #'zerop state))

(defvar *moves* '((2 . 0) (2 . 1)
                  (1 . 0) (1 . 2)
                  (0 . 1) (0 . 2)))

(defun solve (state)
  (let ((table (make-hash-table :test 'equalp)))
    (labels ((rec (state depth moves)
               (declare (type fixnum depth))
               (unless (gethash state table)
                 (when (solvedp state)
                   (return-from solve (values state depth (nreverse moves))))
                 
                 (setf (gethash state table) t)
                 
                 (dolist (m *moves*)
                   (when (valid-move-p state m)
                     (format t "~vT~D: ~S on ~A~%" (* 2 depth) depth m state)
                     (rec (do-move state m) (1+ depth) (cons m moves)))))))
      (rec state 1 nil))))