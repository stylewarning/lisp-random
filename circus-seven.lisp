;;;; circus-seven.lisp
;;;;
;;;; Copyright (c) 2016 Robert Smith

;;; Algorithm:
;;;
;;; The obvious brute force is to try every permutation of every
;;; rotation of every disk. This will definitely lead to a solution
;;; but requires we generate entire solutions and validate
;;; them. Instead we will generate a search tree and prune as early as
;;; possible. Still brute-force, but has the practical effect of
;;; requiring a lot less time.
;;;
;;; One interesting bit of the solution is that we try to pre-compute
;;; as much as possible. For example, instead of rotating a piece
;;; in-pace to determine if it's a candidate, we check if it has
;;; properly adjacent colors. A color adjacency table is computed at
;;; compile-time.
;;;
;;; Finally, we take advantage of the fact that there's rotational
;;; symmetry, and eliminate checking those candidates entirely. (If
;;; they're desired, they can be generated.)
;;;
;;;
;;; NOTE: The code generates a solution but not that which I expect.

(ql:quickload :cl-algebraic-data-type)
(ql:quickload :cl-permutation)

(adt:defdata color
  black
  green
  white
  yellow
  red
  blue)

(defvar *color-assignments*
  (vector black green white yellow red blue)
  "Map between integers [0-5] and colors.")

(defparameter *pieces*
  (vector
   (perm:make-cycle 0 4 1 5 3 2)
   (perm:make-cycle 0 1 2 3 4 5)
   (perm:make-cycle 0 3 1 2 4 5)
   (perm:make-cycle 0 3 1 5 4 2)
   (perm:make-cycle 0 4 2 5 1 3)
   (perm:make-cycle 0 3 4 2 1 5)
   (perm:make-cycle 0 5 2 4 3 1))
  "A list of the pieces.")

(defparameter *adjacency-table*
  (let ((table (make-array '(6 6) :element-type 'fixnum
                                  :initial-element 0)))
    (flet ((record-adjacencies (piece-number)
             (lambda (c1 c2)
               (setf (aref table c1 c2)
                     (dpb 1
                          (byte 1 piece-number)
                          (aref table c1 c2))))))
      (loop :for piece :across *pieces*
            :for piece-number :from 0
            :do (perm::map-cycle-mappings
                 (record-adjacencies piece-number)
                 piece)))
    table)
  "Create a 2D array mapping color pairs to piece indexes (as a bit vector as an integer) with those colors adjacent.")

;;; This is the main criterion for a valid solution. If this is zero,
;;; then there's no piece that fits, and we must backtrack.
(defun pieces-with-adjacent-colors (c1 c2)
  "Return a list pieces with CW-adjacent colors C1 and C2"
  (loop :for i :below 7
        :when (logbitp i (aref *adjacency-table* c1 c2))
          :collect i))

;;; A single piece is numbered as such:
;;;
;;;     0 1
;;;    5   2
;;;     4 3
;;;
;;; A board is a vector whose positions are:
;;;
;;;     1 2
;;;    6 0 3
;;;     5 4

(defun map-remaining (bit-set f)
  (loop :for i :below 7
        :unless (logbitp i bit-set)
          :do (funcall f i)))

(defun center-edge-touching-piece (piece-number)
  (1- piece-number))

(defun piece-edge-touching-center (piece-number)
  (+ 2 piece-number))

(defun piece-edge-touching-CW-piece (piece-number)
  (1+ piece-number))

(defmacro with-piece ((i board bit-set) &body body)
  (assert (symbolp board))
  (assert (symbolp bit-set))
  (let ((gi (gensym "I")))
    `(let ((,gi ,i))
       ;; Only execute if we haven't seen it before.
       (unless (logbitp ,gi ,bit-set)
         (vector-push ,gi ,board)
         (setf ,bit-set (dpb 1 (byte 1 ,gi) ,bit-set))

         ,@body

         (vector-pop ,board)
         (setf ,bit-set (dpb 0 (byte 1 ,gi) ,bit-set))))))

(defun color-position (cycle color)
  (check-type cycle perm:cycle)
  (check-type color (integer 0 5))
  (loop :named search-loop
        :for pos :below 6
        :when (= color (perm:cycle-ref cycle pos))
          :do (return-from search-loop pos)))

(defun find-solutions ()
  (let ((board (make-array 7 :fill-pointer 0)))
    ;; Try every center piece. The orientation of the center piece
    ;; does not matter due to symmetry.
    (loop :for center :across *pieces*
          :for center-number :from 0
          :for rotations := (make-array 7 :initial-element 0)
          :for used-pieces := 0 :do
            (with-piece (center-number board used-pieces)
              (labels ((find-and-place-next-piece (position-to-fill)
                         ;(format t "=============~%")
                         ;(format t "Position being filled: ~D~%" position-to-fill)
                         ;(format t "Board: ~A~%" board)
                         ;(format t "Used pieces: ~B~%" used-pieces)
                         ;(format t "Rotations: ~A~2%" rotations)
                         ;; If we have filled all positions, return our
                         ;; solution.
                         (when (= 7 position-to-fill)
                           ;; maybe need to do a final check between
                           ;; last and first piece.
                           (print board)
                           (return-from find-and-place-next-piece board))
                         ;; We need to fill this position.
                         (let ((center-edge-color
                                 (perm:cycle-ref
                                  center
                                  (center-edge-touching-piece position-to-fill)))
                               (ccw-edge-color
                                 (perm:cycle-ref
                                  (aref *pieces* (aref board (1- position-to-fill)))
                                  (- (aref rotations (1- position-to-fill))
                                     (piece-edge-touching-CW-piece
                                      (1- position-to-fill))))))
                           ;; Find next pieces with the right colors.
                           (let ((next-pieces
                                   (pieces-with-adjacent-colors ccw-edge-color
                                                                center-edge-color)))
                             (dolist (next-piece next-pieces)
                               ;; Place the next piece.
                               (with-piece (next-piece board used-pieces)
                                 ;; Find the rotation. The center color CENTER-EDGE-COLOR should be in position
                                 (let* ((target-color center-edge-color)
                                        (target-position (piece-edge-touching-center position-to-fill))
                                        (current-position (color-position (aref *pieces* next-piece)
                                                                          target-color)))
                                   (setf (aref rotations position-to-fill)
                                         (mod (- target-position current-position) 6))
                                   (find-and-place-next-piece (1+ position-to-fill)))))))))
                ;; Try all other pieces in the first position.
                (dotimes (i 7)
                  (unless (= i center-number)
                    ;; Put the second piece on the board.
                    (with-piece (i board used-pieces)
                      ;; Find the rotation of the piece
                      ;;
                      ;; The center edge to the first piece is always
                      ;; black since all cycles are normalized. Our
                      ;; rotation is how much we need to rotate the cycle
                      ;; so that black in the first piece touches the
                      ;; center.
                      (setf (aref rotations 1) (piece-edge-touching-center 1))
                      
                      ;; Now try filling out the rest of the board.
                      (find-and-place-next-piece 2)))))))))
