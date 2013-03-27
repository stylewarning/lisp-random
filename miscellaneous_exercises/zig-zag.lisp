;;;; zig-zag.lisp
;;;; Copyright (c) 2013 Robert Smith

;;;; Challenge: For an NxN grid, output the numbers in a zig-zag
;;;; pattern as shown here:
;;;;
;;;; http://upload.wikimedia.org/wikipedia/commons/4/43/JPEG_ZigZag.svg
;;;;
;;;; For example, for N = 5,
;;;;
;;;;  0  1  5  6 14
;;;;  2  4  7 13 15
;;;;  3  8 12 16 21
;;;;  9 11 17 20 22
;;;; 10 18 19 23 24

;;;; Solution:
;;;;
;;;; Numbers increase and decrease on alternating diagonals. We can
;;;; check if the numbers are increasing (downward) on a diagonal by
;;;; the sum of the coordinates is zero modulo 2. This is the same as
;;;; checking if the sum is even. We can see this via the following
;;;; code:
;;;;
;;;; CL-USER> (dotimes (y 5)
;;;;            (dotimes (x 5)
;;;;              (if (evenp (+ x y))
;;;;                  (princ "# ")
;;;;                  (princ "  ")))
;;;;            (terpri))
;;;; #   #   # 
;;;;   #   #   
;;;; #   #   # 
;;;;   #   #   
;;;; #   #   # 
;;;;
;;;; Note that increasing X corresponds to moving right and increasing
;;;; Y corresponds to moving *down*, and (0, 0) is our starting
;;;; coordinate. The numbers are decreasing along the "even"
;;;; diagonals, indicated by the hash marks.
;;;;
;;;; Let's first look at the even diagonals only. If we are on any of
;;;; the non-boundary hash marks, say (X, Y), then we can move to the
;;;; next one simply by doing (X+1, Y-1). However, when Y = 0, we
;;;; simply want to stay at zero, and just move to the right, giving
;;;; (X+1, Y). This can be written succinctly as (X+1, max(0, Y-1)).
;;;;
;;;; However, X is on the boundary, X = N-1, and we cannot increase
;;;; anymore. Instead, we simply move down: (X, Y+1).

(defun next (n x y)
  (if (< x (1- n))                      ; Within bounds?
      (values (1+ x) (max 0 (1- y)))    ; Move up-right.
      (values x      (1+ y))))          ; Move down.

;;;; How about for odd diagonals? Actually, the solution is the same,
;;;; but with (X, Y) mapped to (Y, X). That is, when we are on an odd
;;;; diagonal at (X, Y), then
;;;;
;;;;   (NEXT N Y X)
;;;;
;;;; will return (Y_next, X_next), which is trivially flipped to the
;;;; coordinate (X_next, Y_next).
;;;;
;;;; The implementation strategy is simple: create an NxN array, and
;;;; keep track of our coordinates X and Y, starting at the
;;;; origin. When we are on an even diagonal, update X and Y as
;;;; follows:
;;;;
;;;;    X,Y := (NEXT N X Y)
;;;;
;;;; and similarly, when we are on an odd diagonal, update
;;;; accordingly:
;;;;
;;;;    Y,X := (NEXT N Y X).

(defun make-zig-zag (n)
  (let ((result (make-array (list n n) :initial-element 0))
        (x 0)
        (y 0))
    (dotimes (i (* n n) result)
      (setf (aref result x y) i)
      (if (evenp (+ x y))
          (multiple-value-setq (x y) (next n x y))
          (multiple-value-setq (y x) (next n y x))))))

;;;; Finally, to actually print the resulting array, we just loop
;;;; through it each row and column serially. The maximum length of
;;;; any single cell is the number of digits in N*N - 1:
;;;;
;;;;    MAXLEN = 1 + floor( log(N*N - 1) )
;;;;
;;;; where `log' is the common (base-10) logarithm. As such, we print
;;;; every number with at most MAXLEN characters, plus one more for
;;;; the extra space in between numbers.

(defun print-grid (array)
  (let* ((n (array-dimension array 0))
         (maxlen (+ 2 (floor (log (1- (* n n)) 10)))))
    (dotimes (y n)
      (dotimes (x n)
        (format t "~vD" maxlen (aref array x y)))
      (terpri))))

;;;; Finally, we combine everything into the final solution. We make
;;;; sure the argument is a positive integer.

(defun zig-zag (n)
  (assert (and (integerp n)
               (plusp n)))
  (print-grid
   (make-zig-zag n)))
