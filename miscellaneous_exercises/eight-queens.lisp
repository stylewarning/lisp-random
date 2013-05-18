;;;; eight-queens.lisp
;;;; Copyright (c) 2013 Robert Smith

;;;; The classical eight queens problem.

(defstruct queen row col)

(defun queens (n)
  "Create a function which generates new queens for a board of size
N. The resulting function will take the row and column of a queen ROW
and COL, and return a queen at that position."
  (let ((cache (make-hash-table)))
    (lambda (row col)
      (let* ((coord (+ col (* row n)))
             (lookup (gethash coord cache)))
        (if lookup
            lookup
            (setf (gethash coord cache)
                  (make-queen :row row :col col)))))))

(defun attacksp (p q)
  "Does the queen Q attack queen P?"
  (flet ((row= (p q)
           (= (queen-row p) (queen-row q)))
         (col= (p q)
           (= (queen-col p) (queen-col q)))
         (diag= (p q)
           (= (abs (- (queen-row p) (queen-row q)))
              (abs (- (queen-col p) (queen-col q))))))
    (or (row= p q)
        (col= p q)
        (diag= p q))))

(defun safep (new-queen queens)
  "Is the queen NEW-QUEEN safe with the set of queens QUEENS?"
  (notany (lambda (queen)
            (attacksp queen new-queen))
          queens))

(defun solve (n)
  "Compute the solutions to the N-queens problem."
  (let ((queen (queens n)))
    (labels ((rec (row col pos sols)
               (cond
                 ;; We've reached the last row. Accumulate the
                 ;; solutions.
                 ((= row n) (cons (reverse pos) sols))

                 ;; We've reached the last column, return the
                 ;; solutions found so far.
                 ((= col n) sols)
                 
                 ;; Check the next positions. If the queen is safe,
                 ;; add it, and compute the possible arrangements of
                 ;; queens in the next row, and continue by placing
                 ;; the queen in the next column.
                 (t (let ((q (funcall queen row col)))
                      (if (safep q pos)
                          (rec row (1+ col) pos (rec (1+ row) 0 (cons q pos) sols))
                          (rec row (1+ col) pos sols)))))))
      (rec 0 0 nil nil))))

(defun bench (&optional (max 11))
  (dotimes (i max)
    (format t "~A queens problem: ~A~%" (1+ i) (length (solve (1+ i))))))

;;; START
;; User time             =        1.434
;; System time           =        0.004
;; Elapsed time          =        1.454
;; Allocation   = 76066680 bytes

;;; QUEEN CACHING
;; User time             =        1.381
;; System time           =        0.002
;; Elapsed time          =        1.387
;; Allocation   = 4185256 bytes


;;;; MICRO-OPTIMIZATIONS

;;; We can exploit regularities in the solution:
;;;
;;;   * We can pre-allocate solution vectors, since we know each
;;;     solution is a set of N queens.
;;;
;;;   * We only need to store the column of each queen, since the
;;;     ordering of the solutions is fixed. That is, we always have
;;;     queens sorted by row.
;;;
;;;   * We can optimize the structure to use conses instead.
;;;
;;;   * Since recursive backtracking is used, the problem is only
;;;     feasible for smaller numbers. As such, we can take advantage
;;;     of this and optimize for fixnum-sized quantities.
