(defparameter *example* '(( 1  1  1  2  0)
                          ( 3  4  5  6  6)
                          ( 7  4  5  8  9)
                          (10  4 11 12 13)
                          (14 15 15 12 13)))

(defun 2D-list->array (lst)
  (let ((rows (length lst))
        (cols (length (car lst))))
    (make-array (list rows cols) :initial-contents lst)))

(defun flatten (lst)
  (reduce #'append lst))

(defstruct (sbpstate (:conc-name sbp.))
  x
  y
  boards
  last-boards
  new-boards
  num-pieces
  pieces
  piece-nums
  is-a
  a-is
  piece-positions
  )

(defparameter *global* nil)

(defun main ()
  (prog (testboard)
     (setf *global* (make-sbpstate))
     (setf testboard (2D-list->array *example*))

     (let ((dims (array-dimensions testboard)))
       (setf (sbp.x *global*) (first dims))
       (setf (sbp.y *globals* (second dims))))

     (setf (sbp.num-pieces *global*) (length (remove 0 (remove-duplicates (flatten *example*)))))
     )
  )

(defun can-move-up-p (board piece x y)
  (not (or (zerop y)
           (not (or (= piece (aref board (1- y) x))
                    (zerop (aref board (1- y) x)))))))

(defun can-move-down-p (board piece x y)
  (not (or (= y (1- (sbp.y *global*)))
           (not (or (= piece (aref board (1+ y) x))
                    (zerop (aref board (1+ y) x)))))))

(defun can-move-left-p (board piece x y)
  (not (or (zerop x)
           (not (or (= piece (aref board y (1- x)))
                    (zerop (aref board y (1- x))))))))

(defun can-move-right-p (board piece x y)
  (not (or (= x (1- (sbp.x *global*)))
           (not (or (= piece (aref board y (1+ x)))
                    (zerop (aref board y (1+ x))))))))

