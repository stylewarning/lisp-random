;;;; doubly-linked-grid.lisp
;;;; Copyright (c) 2013 Robert Smith

(setf *print-circle* t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-list (n f)
  (loop :repeat n
        :collect (funcall f)))


;;;;;;;;;;;;;;;;;;;;;;;;;;; Cells & Grids ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct cell
  (data  nil :type t)
  (north nil :type (or null cell))
  (east  nil :type (or null cell))
  (south nil :type (or null cell))
  (west  nil :type (or null cell)))

(macrolet ((define-glue (name from-accessor to-accessor)
             (let ((a (gensym "A-"))
                   (b (gensym "B-")))
               `(defun ,name (,a ,b)
                  (unless (null ,a)
                    (setf (,from-accessor ,a) ,b))
                  (unless (null ,b)
                    (setf (,to-accessor ,b) ,a))
                  ;; Return the cell connected.
                  ,b))))
  (define-glue glue-north cell-north cell-south)
  (define-glue glue-east cell-east cell-west)
  (define-glue glue-south cell-south cell-north)
  (define-glue glue-west cell-west cell-east))

(defun make-horizontal-strip (n)
  "Make a horizontal strip of N cells."
  (let ((cells (generate-list n #'make-cell)))
    (reduce #'glue-east cells)
    
    ;; Return the first cell
    (first cells)))

(defun make-vertical-strip (n)
  "Make a vertical strip of N cells."
  (let ((cells (generate-list n #'make-cell)))
    (reduce #'glue-south cells)
    
    ;; Return the first cell
    (first cells)))

(defun glue-horizontal-strips (upper-strip lower-strip)
  "Glue two LOWER-STRIP to the bottom of UPPER-STRIP."
  (loop :for upper-cell := upper-strip :then (cell-east upper-cell)
        :for lower-cell := lower-strip :then (cell-east lower-cell)
        :until (or (null upper-cell) (null lower-cell))
        :do (glue-south upper-cell lower-cell)
        :finally (return upper-strip)))

(defun make-grid (m n)
  "Make a grid of cells, M cells long and N cells tall."
  (reduce (lambda (grid strip)
            (glue-horizontal-strips strip grid))
          (generate-list n (lambda () (make-horizontal-strip m)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;; Grid Traversal ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(macrolet ((define-traversal (name traversal-fn)
             `(defun ,name (cell)
                (declare (optimize speed))
                (if (or (null cell) (null (,traversal-fn cell)))
                    cell
                    (,name (,traversal-fn cell))))))
  (define-traversal traverse-up cell-north)
  (define-traversal traverse-right cell-east)
  (define-traversal traverse-down cell-south)
  (define-traversal traverse-left cell-west))

(defun north-east-corner (cell)
  (traverse-up (traverse-left cell)))

;;; Traversal Directions
;;;
;;;   Simple-Direction = :N | :E | :S | :W
;;;
;;;   Direction  = Simple-Direction | Branch
;;;
;;;   Branch     = NIL
;;;              | (Directions . Branch)
;;;
;;;   Directions = NIL
;;;              | (Direction . Directions)

(defun map-relative-traversal (f cell directions &optional (initial-direction :N))
  "Traverse the cell CELL (which is a part of some grid system),
applying F on each cell visited. The path taken by the traversal is
dictated by DIRECTIONS, which has the following grammar:

    Simple-Direction = :N | :E | :S | :W

    Direction  = Simple-Direction | Branch

    Branch     = NIL
               | (Directions . Branch)

    Directions = NIL
               | (Direction . Directions)

Optionally specify the INITIAL-DIRECTION, which should be a simple
direction, which is north (:N) by default.

Directions are taken *relative* to one another, not *absolutely*. If
we have the grid

     1 2 3
     4 5 6
     7 8 9

then the directions

    :E :E :W :E

starting from 1 will visit

    1, 2, 5, 6, 9

and *not*

    1, 2, 3, 2, 3.
"
  (labels ((direction-function (direction)
             (ecase direction
               ((:N) #'cell-north)
               ((:E) #'cell-east)
               ((:S) #'cell-south)
               ((:W) #'cell-west)))
           
           (direction-heading (direction)
             (ecase direction
               ((:N) 0)
               ((:E) 1)
               ((:S) 2)
               ((:W) 3)))
           
           (heading-direction (heading)
             (aref #(:N :E :S :W) heading))
           
           (relative-to-absolute (current-heading direction)
             (heading-direction
              (mod (+ current-heading (direction-heading direction))
                   4)))
           
           ;; Main traversal function.
           (traverse (cell heading direction directions apply?)
             ;; Call F on the cell only when asked to.
             (when apply?
               (funcall f cell))
             
             ;; Only traverse when we have a direction and when we
             ;; aren't at a dead end.
             (unless (or (null direction) (null cell))
               (etypecase direction
                 ;; Simple direction
                 ((keyword)
                  (let ((absolute-direction (relative-to-absolute
                                             heading
                                             direction)))
                    (traverse (funcall (direction-function absolute-direction)
                                       cell)
                              (direction-heading absolute-direction)
                              (car directions)
                              (cdr directions)
                              t)))
                 
                 ;; Branch
                 ((cons)
                  (mapc (lambda (branch)
                          (traverse cell heading (car branch) (cdr branch) nil))
                        direction)
                  ;; The rest of the directions act as an additional branch.
                  (traverse cell heading (car directions) (cdr directions) nil))))))
    
    ;; Begin the traversal.
    (traverse cell
              (direction-heading initial-direction)
              (car directions)
              (cdr directions)
              t)))

(defun map-grid (f grid)
  "Map F across the nodes of the grid, traveling across rows
  left-to-right, and down columns. F should be a function that takes a
  cell as an argument."
  (labels ((map-row (cell)
             (declare (optimize speed))
             (unless (null cell)
               (funcall f cell)
               (map-row (cell-east cell)))))
    (loop :for row := grid :then (cell-south row)
          :until (null row)
          :do (map-row row)
          :finally (return grid))))

;;;;;;;;;;;;;;;;;;;;;;;;; Testing Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun number-grid (grid)
  (let ((c 0))
    (map-grid (lambda (cell)
                (setf (cell-data cell) (incf c)))
              grid)))
