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
  x           ; Int                                -- width
  y           ; Int                                -- height
  depth       ; Int                                -- depth in BFS
  boards      ; Dictionary(PackedBoard, Move)      -- all boards at depth 0
              ;                                      to depth-2
  last-boards ; Dictionary(PackedBoard, Move)      -- boards at depth-1
  new-boards  ; Dictionary(PackedBoard, Move)      -- boards at depth
  num-pieces  ; Int                                -- total pieces
  pieces      ; UnpackedNumber -> PieceDescription --
  piece-nums  ; Shape -> Int                       -- number of pieces
              ;                                      of each shape
  is-a        ; UnpackedNumber -> Ordinal * Shape  --
  a-is        ; Ordinal * Shape -> UnpackedNumber
  shape-positions ; Shape * Ordinal -> Int         -- Gives location of the
                  ;                                  Nth piece w/certain shape
  )

(defun positions (val lst)
  (loop
     :for i := 0 :then (+ i p 1)
     :and p := (position val lst) :then (position val lst :start i)
     :while p :collect p :into tot
     :finally (return (cdr tot))))

(defun find-pieces (brd)
  (let* ((b (flatten brd))
         (num-ids (apply #'max b)))
    (loop
       :for i :from 1 :to num-ids
       :collect (positions i b))))

(defun same-shape (s1 s2)
  (and (= (length s1) (length s2))
       (apply #'= (mapcar #'- s1 s2))))


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

