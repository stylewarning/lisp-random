;;;; battleship.lisp
;;;; Copyright (c) 2013 Robert Smith

;;;; An attempt at a Battleship AI.

(quote #.(ql:quickload :cl-algebraic-data-type))

(defconstant +board-width+ 10
  "The width of the Battleship board.")

(defconstant +board-height+ 10
  "The height of the Battleship board.")

(defconstant +board-size+ (* +board-width+ +board-height+)
  "The total number of cells of a Battleship board.")

(deftype ship-kind ()
  "The kind of ship is identified by a non-negative integer."
  'unsigned-byte)


;;; The state of the opponent's board:
(adt:defdata state
  ;; We have not fired at this position.
  unknown

  ;; We fired at this position and missed. 
  miss

  ;; We have hit a ship, but it has not sunk.
  hit

  ;; We fired at this position and the ship of the indicated ship kind
  ;; as sunk.
  (sunk ship-kind))


;;; The modes by which we choose how to fire our next shot.
(adt:defdata mode
  ;; We have no leads on the next place to fire.
  scavenge
  
  ;; We have hit a ship but are not sure where of the orientation of
  ;; the ship.
  lock-on
  
  ;; We have a guess of the orientation, and will eliminate in that
  ;; direction.
  eliminate)


;;; The possible axial directions.
(adt:defdata direction
  horizontal
  vertical)


;;; The state of our opponent.
(defstruct opponent
  (tracking-board (make-array +board-size+
                              :element-type 'state
                              :initial-element unknown))
  (current-mode scavenge :type mode)
  (probability (make-array +board-size+))
  (last-hits nil :type list)
  lead-direction
  (ships-left (list 1 1 2 2 3 4 5)))


(defun board-ref (board x y)
  (aref board (+ x (* y +board-width+))))

(defsetf board-ref (board x y) (new-value)
  `(setf (aref ,board (+ ,x (* ,y +board-width+))) ,new-value))

(defun linear-to-planar (linear-coordinate)
  (multiple-value-bind (quo rem) (floor linear-coordinate +board-width+)
    (values rem quo)))

(defmacro doboard ((x y &optional result) &body body)
  `(dotimes (,y +board-height+ ,result)
     (dotimes (,x +board-width+)
       ,@body)))

;; broken
(defun print-board (board)
  (dotimes (y +board-height+)
    (dotimes (x +board-width+)
      (let ((value (board-ref board x y)))
        (cond
          ((integerp value) (format t " ~2,' D " value))
          ((null value) (format t "   /"))
          ((eq t value) (format t "   X")))))
    (terpri)))


(defun in-bounds-p (board x y)
  (declare (ignore board))
  (and (<= 0 x (1- +board-width+))
       (<= 0 y (1- +board-height+))))


(defun zero-out-probability (opponent)
  (loop :with board := (opponent-probability opponent)
        :for i :below +board-size+
        :do (setf (aref board i) 0)))

(defun update-probability-single (opponent ship)
  (flet ((in-range-p (x y dir)
           (adt:match direction dir
             (horizontal
              (loop :for i :below ship
                    :for piece := (board-ref (opponent-tracking-board opponent)
                                             (+ x i)
                                             y)
                    :thereis (adt:match state piece
                               (miss     t)
                               ((sunk _) t)
                               (_        nil))))
             (vertical
              (loop :for i :below ship
                    :for piece := (board-ref (opponent-tracking-board opponent)
                                             x 
                                             (+ y i))
                    :thereis (adt:match state piece
                               (miss     t)
                               ((sunk _) t)
                               (_        nil)))))))
    (let ((prob (opponent-probability opponent))
          (track (opponent-tracking-board opponent)))
      
      ;; Increment the tallies.
      (doboard (x y)
        ;; Tally horizontally.
        (when (and (in-bounds-p track (1- (+ x ship)) y)
                   (not (in-range-p x y horizontal)))
          (dotimes (offset ship)
            (incf (board-ref prob (+ x offset) y))))
        
        ;; Tally vertically.
        (when (and (< 1 ship)
                   (in-bounds-p track x (1- (+ y ship)))
                   (not (in-range-p x y vertical)))
          (dotimes (offset ship)
            (incf (board-ref prob x (+ y offset))))))
      
      ;; Post-process: set hits and sunks to zero.
      (doboard (x y opponent)
        (adt:match state (board-ref track x y)
          (hit      (setf (board-ref prob x y) 0))
          (_        0))))))

(defun update-probability (opponent)
  (mapc (lambda (ship)
          (update-probability-single opponent ship))
        (opponent-ships-left opponent))
  opponent)

(defun maximal-position (array)
  (loop :with max-val := -1
        :and  max-pos := -1
        :for pos :below (length array)
        :for val := (aref array pos)
        :do (when (> val max-val)
              (setf max-val val
                    max-pos pos))
        :finally (return max-pos)))

(defun set-miss (opponent x y)
  (setf (board-ref (opponent-tracking-board opponent) x y) miss))

(defun set-hit (board x y)
  (setf (board-ref board x y) hit))

(defun play-move (opponent query-move)
  (adt:match mode (opponent-current-mode opponent)
    (scavenge
     ;; Find most probable point.
     (update-probability opponent)
     
     ;; Find maximal point.
     (multiple-value-bind (x y)
         (maximal-position (opponent-probability opponent))
       
       ;; Receive result.
       (adt:match state (funcall query-move x y)
         (miss
          (setf (board-ref (opponent-tracking-board opponent) x y) miss))

         (hit
          (setf (board-ref (opponent-tracking-board opponent) x y) hit
                (opponent-current-mode opponent) lock-on)
          (push (cons x y) (opponent-last-hits opponent))
          )

         ((sunk ship)
          (if (= 1 ship)
              (error "Unexpected ship sinkage: #~D at (~D, ~D)." ship x y)
              (setf (board-ref (opponent-tracking-board opponent) x y)
                    (sunk ship)

                    (opponent-ships-left opponent)
                    (delete ship (opponent-ships-left opponent)))))

         (unknown
          (error "UNKNOWN is an invalid response for query (~D, ~D)." x y)))
         )
     )
    
    (lock-on
     ;; ...
     )
    
    (eliminate
     ;; ...
     )))
