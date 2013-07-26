;;;; doubly-linked-grid.lisp
;;;; Copyright (c) 2013 Robert Smith

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

