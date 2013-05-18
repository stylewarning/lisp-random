;;;; eight-queens.lisp
;;;; Copyright (c) 2013 Robert Smith

;;;; The classical eight queens problem.

(defstruct queen
  row
  col)

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
  (notany (lambda (queen)
            (attacksp queen new-queen))
          queens))

(defun solve (n)
  (labels ((rec (x y pos sols)
             (cond
               ((> x n) (cons (reverse pos) sols))
               ((> y n) sols)
               (t (let ((q (make-queen :row x :col y)))
                    (if (safep q pos)
                        (rec x (1+ y) pos (rec (1+ x) 1 (cons q pos) sols))
                        (rec x (1+ y) pos sols)))))))
    (rec 1 1 nil nil)))

(defun bench (&optional (max 11))
  (dotimes (i max)
    (format t "~A queens problem: ~A~%" i (length (solve (1+ i))))))
