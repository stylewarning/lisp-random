;;;; circus-seven.lisp
;;;;
;;;; Copyright (c) 2016 Robert Smith

;;; Instructions:
;;;
;;;     (mapc #'pretty-print-solution (mapc #'validate (time (find-solutions))))
;;;
;;; Algorithm:
;;;
;;; The obvious brute force is to try every permutation of every
;;; rotation of every disk. This leads to 7!*6^7 possible
;;; positions. We don't need to try every rotation of the center piece
;;; though due to rotational symmetry, bringing the count to 7!*6^6.
;;;
;;; If we assume that each piece's edges are colored by a permutation
;;; of 6 colors, then we needn't check 6^6 of those positions, since
;;; given a fixed center, only one of those rotations will produce a
;;; valid matching with the center.
;;;
;;; This leaves 7! = 5040 positions to check at most. This is plenty
;;; easy to brute force very quickly by checking every permutation and
;;; checking validity of each. We however take a different
;;; approach. We do tree search with aggressive pruning. We start by
;;; selecting a center piece along with a first matching piece
;;; (trivial), and then following by filling the board clockwise. Each
;;; new piece must satisfy two constraints: the edge touching the
;;; center piece must match, and the edge touching the last filled
;;; piece must match. We use these criteria to do pruning.
;;;
;;; To make pruning fast, we index all pieces' adjacent colors. This
;;; way we always select a piece which satisfies the constraint,
;;; though there is possibility no such piece exists, in which case we
;;; need to backtrack, possibly back to selecting a new center.
;;;
;;; When we have filled a board entirely, we have to check one final
;;; constraint: that the last piece put in matches the first piece put
;;; in within the ring. This is not guaranteed by the above procedure.

(ql:quickload :cl-permutation)
(ql:quickload :global-vars)

;;; Piece representation.
;;;
;;; A "piece" is the main object of the game. It is a hexagon whose
;;; edges are colored. Colors are represented by digits between 0 and
;;; 5. The piece itself is represented as a permutation, specifically
;;; in cycle form.
;;;
;;; The cycle form was chosen because of simplistic indexing and
;;; rotating. Also because, mathematically, a syntactically rotated
;;; cycle represents the same piece.
;;;
;;; A single piece is numbered as such:
;;;
;;;     0 1
;;;    5   2  <==> (0 1 2 3 4 5)
;;;     4 3

(global-vars:define-global-parameter **pieces**
  (vector
   (perm:make-cycle 0 4 1 5 3 2)
   (perm:make-cycle 0 1 2 3 4 5)
   (perm:make-cycle 0 3 1 2 4 5)
   (perm:make-cycle 0 3 1 5 4 2)
   (perm:make-cycle 0 4 2 5 1 3)
   (perm:make-cycle 0 3 4 2 1 5)
   (perm:make-cycle 0 5 2 4 3 1))
  "A vector of the pieces.")

(global-vars:define-global-parameter **num-pieces** (length **pieces**))

;;; For convenience, pieces are referred to by their index (called the
;;; "piece ID") in the vector of which they're a part.

(defun lookup-piece (piece-id)
  "Lookup a piece by its ID."
  (aref **pieces** piece-id))

;;; Sometimes we will need to find the position of a color within a
;;; piece to calculate how much the piece needs to be rotated.
(defun color-position (piece color)
  "What is the position of the color COLOR (an integer) within the piece PIECE?"
  (loop :named search-loop
        :for pos :below 6
        :when (= color (perm:cycle-ref piece pos))
          :do (return-from search-loop pos)))

;;; To speed up the pruning, we make a table containing all color
;;; adjacencies.

(global-vars:define-global-var **adjacency-table**
  (let ((table (make-array '(6 6) :element-type '(unsigned-byte 8)
                                  :initial-element 0)))
    (flet ((record-adjacencies (piece-number)
             (lambda (c cw-c)
               (setf (aref table c cw-c)
                     (dpb 1
                          (byte 1 piece-number)
                          (aref table c cw-c))))))
      (loop :for piece :across **pieces**
            :for piece-number :from 0
            :do (perm::map-cycle-mappings
                 (record-adjacencies piece-number)
                 piece)))
    table)
  "Create a 2D array mapping color pairs to piece indexes (as a bit vector as an integer) with those colors CW-adjacent.")

(defun pieces-with-adjacent-colors (c1 c2)
  "Return a list pieces containing colors C1 and C2 such that C2 is CW-adjacent to C1."
  (loop :for i :below **num-pieces**
        :when (logbitp i (aref **adjacency-table** c1 c2))
          :collect i))

;;; A board is a vector whose positions are:
;;;
;;;     1 2
;;;    6 0 3
;;;     5 4
;;;
;;; These indexes are called "piece numbers" (not to be confused with
;;; their IDs). The piece numbered 0 is the "center" and the pieces
;;; numbered 1 through 6 constitute the "ring".
;;;
;;; First we write a few functions to locate specific edges of a piece
;;; relative to other pieces.

(defun center-edge-touching-piece (piece-number)
  "Which edge of the center touches the piece numbered PIECE-NUMBER?"
  (1- piece-number))

(defun piece-edge-touching-center (piece-number)
  "For a non-center piece numbered PIECE-NUMBER, which edge touches the center piece?"
  (mod (+ 2 piece-number) 6))

(defun piece-edge-touching-CW-piece (piece-number)
  "For a non-center piece numbered PIECE-NUMBER, which edge touches the adjacent CW piece?"
  (mod (1+ piece-number) 6))

(defun piece-edge-touching-CCW-piece (piece-number)
  "For a non-center piece numbered PIECE-NUMBER, which edge touches the adjacent CCW piece?"
  (mod (+ 3 piece-number) 6))

;;; A solution is a combination of piece locations along with their
;;; rotations. As stated above, the rotations could be calculated
;;; on-the-fly, however, we memoize this information within the
;;; implementation of the algorithm, so we save it anyway. This also
;;; makes read-out easier.

(defstruct (solution (:constructor %make-solution))
  "A data structure to hold a solution encoding. A solution is a permutation of pieces on a board along with each piece's rotation (encoded as number of clockwise rotations)."
  board
  rotations)

(defun make-solution (board rotations)
  (%make-solution :board (copy-seq board)
                  :rotations (copy-seq rotations)))

;;; When actually searching for a solution, we have a record of the
;;; current state of the board, along with the pieces that have been
;;; uses so far, ecoded as a bit set in an integer. We want a
;;; convenient way to try a new piece on the board. Below is a macro,
;;; WITH-PIECE, which first checks if that piece has been used. If it
;;; has, then the body code is not executed. If it hasn't, it puts the
;;; piece on the board and marks that piece as used. It'll execute any
;;; given code, and then undo the use of that piece for the purposes
;;; of backtracking.
;;;
;;; This macro would not need to exist if purely functional data
;;; structures were used along with recursion.

(defmacro with-piece ((i board bit-set) &body body)
  "Given the board state BOARD and the bit set of pieces used BIT-SET, along with a piece ID I, place the piece on the board and execute BODY, assuming the piece hasn't been used before. After executing body, take it back off the board."
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

;;; Finally, we have the solution-as-pruned-search.

(defun find-solutions ()
  "Find all solutions to the Circus Seven puzzle as encoded by **PIECES**. Return a list of SOLUTION structures."
  (let (
        ;; The maintained state of the tree search.
        (board       (make-array **num-pieces** :fill-pointer 0))
        (used-pieces 0)                 ; bit set
        (rotations   (make-array **num-pieces** :initial-element 0))
        ;; Collected solutions and statistics.
        (placements  0)
        (solutions   nil))
    ;; Try every center piece. The orientation of the center piece
    ;; does not matter due to symmetry.
    (dotimes (center **num-pieces**)
      (with-piece (center board used-pieces)
        (incf placements)
        (labels ((final-constraint-satisfied-p (board rotations)
                   "Given a completed board BOARD and the associated rotations ROTATIONS, does the first placed piece in the ring match the last placed piece in the ring?"
                   (= (perm:cycle-ref (lookup-piece (aref board 1))
                                      (- 4 (aref rotations 1)))
                      (perm:cycle-ref (lookup-piece (aref board 6))
                                      (- 1 (aref rotations 6)))))
                 (find-and-place-next-piece (position-to-fill)
                   ;; If we have filled all positions, check the final
                   ;; constraint. If that is satisfied, record our
                   ;; solution.
                   (when (= **num-pieces** position-to-fill)
                     ;; Check our very last constraint before adding
                     ;; the purported solution.
                     (when (final-constraint-satisfied-p board rotations)
                       (push (make-solution board rotations) solutions))

                     ;; Backtrack a step to continue the search.
                     (return-from find-and-place-next-piece board))

                   ;; We aren't done with the search. Compute the
                   ;; color constraints for this piece. These are the
                   ;; center color and the color of the coincident
                   ;; edge of the previously-placed piece.
                   (let* ((last-piece-position (1- position-to-fill))
                          (next-piece-position (1+ position-to-fill))
                          (center-edge-color
                            (perm:cycle-ref
                             (lookup-piece center)
                             (center-edge-touching-piece position-to-fill)))
                          (ccw-edge-color
                            (perm:cycle-ref
                             (lookup-piece (aref board last-piece-position))
                             (- (piece-edge-touching-CW-piece last-piece-position)
                                (aref rotations last-piece-position)))))
                     ;; Find next pieces with the right colors.
                     (let ((next-pieces
                             (pieces-with-adjacent-colors center-edge-color
                                                          ccw-edge-color)))
                       (dolist (next-piece next-pieces)
                         ;; Place the next piece.
                         (with-piece (next-piece board used-pieces)
                           (incf placements)
                           ;; Find the rotation by looking up a color
                           ;; offset.
                           (let* ((target-color center-edge-color)
                                  (target-position (piece-edge-touching-center
                                                    position-to-fill))
                                  (current-position (color-position
                                                     (lookup-piece next-piece)
                                                     target-color)))
                             (setf (aref rotations position-to-fill)
                                   (mod (- target-position current-position) 6))
                             ;; Descend in the tree search by trying
                             ;; to place another piece.
                             (find-and-place-next-piece next-piece-position))))))))
          ;; Try all other pieces in the first ring position.
          (dotimes (first-ring **num-pieces**)
            (with-piece (first-ring board used-pieces)
              (incf placements)
              ;; Find the rotation of the piece.
              ;;
              ;; The center edge to the first piece is always black
              ;; since all cycles are normalized. Our rotation is how
              ;; much we need to rotate the cycle clockwise so that
              ;; black in the first piece touches the center.
              (setf (aref rotations 1) (piece-edge-touching-center 1))

              ;; Now try filling out the rest of the board.
              (find-and-place-next-piece 2))))))

    ;; Print out some useful info about the search.
    (format t "Tried ~D piece placements. Found ~D solutions.~%"
            placements
            (length solutions))

    ;; Return the solutions.
    solutions))


;;; For testing/validation only.

(defun validate (sol)
  (let* ((board (solution-board sol))
         (rotations (solution-rotations sol))
         ;; Extract the pieces.
         (center (lookup-piece (aref board 0)))
         (ring   (map 'list #'lookup-piece (subseq board 1))))
    ;; Check that all pieces match the center.
    (loop :for tile :in ring
          :for i    :from 1
          :for rotated-tile := (perm:rotate-cycle tile (- (aref rotations i)))
          :do (assert (= (perm:cycle-ref center (1- i))
                         (perm:cycle-ref rotated-tile (piece-edge-touching-center i)))
                      nil
                      "Outer ring tile ~D doesn't match center." i))
    ;; Check that all pieces in the ring match each other.
    (loop :for x :in ring
          :for y :in (rest ring)
          :for xi :from 1
          :for yi :from 2
          :for rotated-x := (perm:rotate-cycle x (- (aref rotations xi)))
          :for rotated-y := (perm:rotate-cycle y (- (aref rotations yi)))
          :do (assert (= (perm:cycle-ref rotated-x (piece-edge-touching-cw-piece xi))
                         (perm:cycle-ref rotated-y (piece-edge-touching-ccw-piece yi)))
                      nil
                      "Tile ~D doesn't match its CW edge with tile ~D." xi yi))))


;;; For pretty printing the solution.

(defun number->color (n)
  "Convert an integer N representing a color to a color name (represented as a symbol)."
  (aref #(black green white yellow red blue) n))

(defun cycle->list (cyc)
  "Convert a cycle CYC to a list."
  (loop :for i :below (perm:cycle-length cyc)
        :collect (perm:cycle-ref cyc i)))

(defun pretty-print-solution (sol)
  "Pretty print the solution SOL to *STANDARD-OUTPUT*."
  (let ((board (map 'list (lambda (piece rot)
                            (mapcar #'number->color
                                    (cycle->list
                                     (perm:rotate-cycle (lookup-piece piece)
                                                        (- rot)))))
                    (solution-board sol)
                    (solution-rotations sol))))
    (loop :for pos :in '("Center      "
                         "Top Left    "
                         "Top Right   "
                         "Right       "
                         "Bottom Right"
                         "Bottom Left "
                         "Left        ")
          :for piece :in board
          :do (format t "~A: ~{~A~^, ~}~%"pos  piece)
          :finally (terpri))))
