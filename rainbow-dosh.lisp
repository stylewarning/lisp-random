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

(defun reduce-state (state)
  "Reduce STATE to contain the smallest numbers possible."
  (let* ((a (aref state 0))
         (b (aref state 1))
         (c (aref state 2))
         (gcd (gcd a b c)))
    (make-state (floor a gcd)
                (floor b gcd)
                (floor c gcd))))

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

(defun almost-solved-p (state)
  "Check if STATE is almost solved, that is, if it has two equal piles."
  (declare (type state state))
  (let ((a (aref state 0))
        (b (aref state 1))
        (c (aref state 2)))
    ;; We needn't check if A = C because if that is the case, and we
    ;; have A <= B <= C, then that imples A = B = C.
    (cond
      ((= a b) (make-move 1 0))
      ((= b c) (make-move 2 1))
      (t nil))))

;;; We could have 6 moves here, however, since we have A <= B <= C, we
;;; will never swap lower to higher.
(defvar *moves* '((2 . 0) (2 . 1) (1 . 0)))

(defun solve (state)
  (let ((table (make-hash-table :test 'equalp)))
    (labels ((rec (state moves)
               ;; Have we reached this state yet? If not, skip it.
               (unless (gethash state table)
                 ;; Is it solved? If so, return the solution and the
                 ;; final state.
                 (when (solvedp state)
                   (return-from solve (values (nreverse moves) state)))
                 
                 ;; Mark this position as visited.
                 (setf (gethash state table) t)
                 
                 ;; Avoid trying all moves when we know we are in the
                 ;; winning state.
                 (let ((winning-move (almost-solved-p state)))
                   (when winning-move
                     (rec (do-move state winning-move) (cons winning-move moves))))
                 
                 ;; Brute force... try all moves.
                 (dolist (m *moves*)
                   (when (valid-move-p state m)
                     (rec (do-move state m) (cons m moves)))))))
      (rec state nil))))

(defun apply-moves (initial-state moves)
  (loop :for move :in moves
        :for state := (do-move initial-state move)
                   :then (do-move state move)
        :collect state))

(defun solve-and-print (puzzle-state)
  (let ((solution (solve puzzle-state)))
    (format t "Start   --> ~A~%" puzzle-state)
    (loop :for move :in solution
          :for state :in (apply-moves puzzle-state solution)
          :do (format t "~A --> ~A~%" move state))))