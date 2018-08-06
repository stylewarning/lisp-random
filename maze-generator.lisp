;;;; maze-generator.lisp
;;;;
;;;; Copyright (c) 2018 Robert Smith

;;; To load this file: (load "maze-generator.lisp")
;;;
;;; To use, use RANDOM-MAZE and DRAW-MAZE. The canonical start
;;; position is (0, 0) and end position is (ROWS - 1, COLS - 1).
;;;
;;; CL-USER> (time (draw-maze (random-maze 20 40)))
;;; ┏┳━┳━┳━━┳━┳━━┳━━━┳┳━┳━━┳━━┳━━━┳━━━━━━━━┳┓
;;; ┃┃╻╹╻┃╻╺┛╻┃╺┓╹┏━┓┗┛╻┃╻╻┗┓╻╹╻┏━┫╺━┳━━━╸╻╹┃
;;; ┃┃┣━┫┃┣━┳┛┃╻┗┓┃╻┗┳━┛┗┛┣┓┃┗━┻┛╻┣━┓┃┏━━━┫╻┃
;;; ┃┃┃╻╹┃┃╻┃┏┛┗┓┗┛┣╸┣━┳━━┛┃┗━┳━━┫╹╻┃┃┃┏━┓┃┃┃
;;; ┃┗┫┃┏┛╹┣┛┃╺━╋━━┫╺┫╻┗┓╺━┻━┓┗┓╻┗━┛┃┃╹┃╻┃┃┃┃
;;; ┣┓┃┣┛┏━┫╺┻━┓┣╸╻┃╺┫┃╻┣━━┳╸┗┓┃┣━┳━┛┃╺┻┫┃┗┫┃
;;; ┃┃┃╹╺┻┓┗━━┓╹╹┏┻╋╸┃┃┃┗━┓┃┏┓┃┃╹╻╹┏━┻━┓┃┗┓┗┫
;;; ┃┃┗┳━╸┣━━┓┣━┳┫╻┃╺┛┃┗━┓┃┃╹┃┃┣━┻━╋━╸╻┃┗╸┃╻┃
;;; ┃┗╸┃╺━┻╸╻╹┗╸┃╹┃┣━╸┣━━┛┃┣┓┃╹┃╺┳┓┃╺┓┃┗━┳┻┫┃
;;; ┃┏━╋━━━┳┫┏━━┛┏┛┣━━┛╻┏━┛┃╹┗━┻╸┃╹┣━┛┣┳╸┃╻╹┃
;;; ┃┗┓┃┏━┓┃╹┃┏━━┛╻╹┏━┳┛┃╻╺┻━━━━┳┛┏┛┏┳┛╹┏┛┣╸┃
;;; ┣┓┃╹┃╺┫┗━┫┗━┓┏┻━┛╻┣━┻┫┏━━━┳╸┃┏┛┏┛┃┏━┫┏┛╻┃
;;; ┃┃┗┓┃╻┗━┓┃╺┓┗┛╺┳━┛┃╺┓┃┃╺━━┫╺┛┃┏┻╸┃┗╸┃┗━┫┃
;;; ┃┗┓╹┣┻╸╻┃┣━┻━┳╸┃┏━┻╸┃┃┗┳━┓┗┳━┛┃╻╺╋━╸┃╺┓┃┃
;;; ┃┏┻┳┛╺┳┛┃╹┏━┓┃┏┛┃┏━━┫┗┓╹╻┣╸┃╻┏┻┻┓╹┏━┻━┫┗┫
;;; ┃╹╻┃╺┓┃┏┻━┛╻┃┣┛╻┃┃┏┓┗╸┣━┛┃╻┃┃┃┏╸┃┏┛╺┳╸┗╸┃
;;; ┣━┛┃╺┫┃┃╻┏━┫╹┃╺┻┛┃┃┗━┳┛┏━┫┗┫┃┃┗━┛┗━╸┃┏━━┫
;;; ┃╺┳┻╸┃┣┛┃┗┓┗━┻┓┏━┛┣━╸┃┏┛╻┗╸┃┃┗━━━━┳━┫╹┏┓┃
;;; ┣╸╹┏━┫┃╺╋┓┗┓╺┓┃┃┏┓┃╺━┛┃┏┻╸╻┃┣━━━━╸┃╺┻┓┃╹┃
;;; ┃╺━┛╻┃┗╸┃┗╸┗╸┃┗┛┃╹┃╺━━┻┛┏━┛┗┛┏━━━━┛╺┓╹┃╺┫
;;; ┗━━━┻┻━━┻━━━━┻━━┻━┻━━━━━┻━━━━┻━━━━━━┻━┻━┛
;;; Evaluation took:
;;;   0.001 seconds of real time
;;;   0.001328 seconds of total run time (0.001316 user, 0.000012 system)
;;;   100.00% CPU
;;;   3,705,428 processor cycles
;;;   65,504 bytes consed

(ql:quickload :alexandria)              ; for SHUFFLE

;;; A good rectilinear maze generator probably has the following
;;; properties:
;;;
;;;     - There are no "shortcuts". This amounts to meaning there are
;;;       no cycles or loops. This is to say that there is exactly one
;;;       and only one path between every two reachable cells of the
;;;       maze.
;;;
;;;     - Every cell should be reachable by default, unless certain
;;;       squares are explicitly blocked off.
;;;
;;;     - The generator should be relatively fast and at least
;;;       be able to generate 100 x 100 mazes.


;;;;;;;;;;;;;;;;;;;;;;;; Maze Representation ;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A maze is a rectangular array of cells.
;;;
;;; The state of a cell is a collection of bits where the first bit
;;; determines if the cell has been visited, and the rest of the bits
;;; determine the cardinal directions one can travel from this cell.
(macrolet ((defbitfield (name &rest accessors)
             `(progn
                (deftype ,name () '(mod ,(expt 2 (length accessors))))
                ,@ (loop :for i :from 0
                         :for (getter setter) :in accessors
                         :collect `(defun ,getter (,name) (logbitp ,i ,name))
                         :collect `(defun ,setter (,name) (dpb 1 (byte 1 ,i) ,name))))))
  (defbitfield cell
    ;; Has this cell been visited?
    (visited? set-visited)
    ;; These test/set whether each of the cardinal directions are
    ;; open.
    (north?  set-north)
    (east?   set-east)
    (south?  set-south)
    (west?   set-west)))

;;; A cell that is closed and unvisited.
(defconstant +closed-cell+ 0)

(deftype maze () `(simple-array cell (* *)))

(defun make-maze (rows cols)
  (make-array (list rows cols)
              :element-type 'cell
              :initial-element +closed-cell+))

(defun maze-rows (maze)
  (array-dimension maze 0))

(defun maze-cols (maze)
  (array-dimension maze 1))

(defun num-cells (maze)
  (* (maze-rows maze) (maze-cols maze)))

(defun visited-cell? (maze row col)
  (visited? (aref maze row col)))

;;; Is (ROW, COL) on the top/bottom/left/right wall of MAZE?
(defun top-cell? (maze row col)
  (declare (ignore maze col))
  (zerop row))

(defun bottom-cell? (maze row col)
  (declare (ignore col))
  (= row (1- (maze-rows maze))))

(defun left-cell? (maze row col)
  (declare (ignore maze row))
  (zerop col))

(defun right-cell? (maze row col)
  (declare (ignore row))
  (= col (1- (maze-cols maze))))

;;; Can we travel in these directions?
(defun can-travel-north? (maze row col)
  (not (or (top-cell? maze row col)
           (visited-cell? maze (1- row) col))))

(defun can-travel-east? (maze row col)
  (not (or (right-cell? maze row col)
           (visited-cell? maze row (1+ col)))))

(defun can-travel-south? (maze row col)
  (not (or (bottom-cell? maze row col)
           (visited-cell? maze (1+ row) col))))

(defun can-travel-west? (maze row col)
  (not (or (left-cell? maze row col)
           (visited-cell? maze row (1- col)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; Maze Carving ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We start with a maze with all walls up. Then to generate the maze,
;;; we traverse from our starting cell (0, 0) in random directions. We
;;; don't traverse iff we'd be traversing through the maze boundary or
;;; we'd be traversing to an already-visited cell. This guarantees
;;; every cell is seen once through one and only one path. We stop
;;; traversing as soon as we've visited every cell.

(defun random-visit-order ()
  (alexandria:shuffle '(N E S W)))

(defun initialize-maze (maze)
  (let ((num-visited 0))
    (labels ((carve (r c)
               (assert (not (visited-cell? maze r c)))
               ;; Note that we've visited the cell.
               (setf (aref maze r c) (set-visited (aref maze r c)))
               (incf num-visited)
               ;; Is we've visited everything, we're done!
               (when (= num-visited (num-cells maze))
                 (return-from initialize-maze maze))
               ;; Generate a random visit order and visit!
               (let ((visit-order (random-visit-order)))
                 (dolist (direction visit-order maze)
                   (ecase direction
                     (N (when (can-travel-north? maze r c)
                          (let ((new-r (1- r)) (new-c c))
                            (setf (aref maze r c)         (set-north (aref maze r c)))
                            (setf (aref maze new-r new-c) (set-south (aref maze new-r new-c)))
                            (carve new-r new-c))))
                     (E (when (can-travel-east? maze r c)
                          (let ((new-r r)      (new-c (1+ c)))
                            (setf (aref maze r c)         (set-east (aref maze r c)))
                            (setf (aref maze new-r new-c) (set-west (aref maze new-r new-c)))
                            (carve new-r new-c))))
                     (S (when (can-travel-south? maze r c)
                          (let ((new-r (1+ r)) (new-c c))
                            (setf (aref maze r c)         (set-south (aref maze r c)))
                            (setf (aref maze new-r new-c) (set-north (aref maze new-r new-c)))
                            (carve new-r new-c))))
                     (W (when (can-travel-west? maze r c)
                          (let ((new-r r)      (new-c (1- c)))
                            (setf (aref maze r c)         (set-west (aref maze r c)))
                            (setf (aref maze new-r new-c) (set-east (aref maze new-r new-c)))
                            (carve new-r new-c)))))))))
      (carve 0 0))))

(defun random-maze (rows cols)
  (initialize-maze (make-maze rows cols)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; Maze Drawing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; To draw, we iterate across all *corners* of the maze, of which
;;; there are (1 + ROWS)*(1 + COLS). We check if there's a wall in
;;; each direction of the corner by checking adjacent cells to that
;;; wall, and we-use our CELL bitfield structure to record this
;;; information, effectively ignoring the VISITED bit. We call each
;;; wall a "spine" of the corner, just to emphasize we are working
;;; with corners, and not cells.

(defvar *box-chars* #(" " "╹" "╺" "┗"
                      "╻" "┃" "┏" "┣"
                      "╸" "┛" "━" "┻"
                      "┓" "┫" "┳" "╋"))

(defun north-spine? (maze r c)
  (and (plusp r)
       (or (zerop c)
           (= c (maze-cols maze))
           (not (and (east? (aref maze (1- r) (1- c)))
                     (west? (aref maze (1- r) c)))))))

(defun east-spine? (maze r c)
  (and (< c (maze-cols maze))
       (or (zerop r)
           (= r (maze-rows maze))
           (not (and (north? (aref maze r c))
                     (south? (aref maze (1- r) c)))))))

(defun south-spine? (maze r c)
  (and (< r (maze-rows maze))
       (or (zerop c)
           (= c (maze-cols maze))
           (not (and (west? (aref maze r c))
                     (east? (aref maze r (1- c))))))))

(defun west-spine? (maze r c)
  (and (plusp c)
       (or (zerop r)
           (= r (maze-rows maze))
           (not (and (south? (aref maze (1- r) (1- c)))
                     (north? (aref maze r (1- c))))))))


(defun draw-maze (maze)
  (dotimes (r (1+ (maze-rows maze)))
    (dotimes (c (1+ (maze-cols maze)))
      (let ((spine 0))
        (when (north-spine? maze r c) (setf spine (set-north spine)))
        (when (east-spine?  maze r c) (setf spine (set-east  spine)))
        (when (south-spine? maze r c) (setf spine (set-south spine)))
        (when (west-spine?  maze r c) (setf spine (set-west  spine)))
        (write-string (aref *box-chars* (ash spine -1)))))
    (terpri)))
