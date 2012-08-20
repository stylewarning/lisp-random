(defun coin-flip ()
  "Choose true or false at random."
  (zerop (random 2)))

(defun random-point-in-rectangle (width height)
  "Choose a random point in this rectangle:

   (0,0)                            (width, 0)
     +-------------------------------+
     |###############################|
     |###############################|
     |###############################|
     +-------------------------------+
   (0, height)                      (width, height)
"
  (list (random width) (random height)))

(defun random-point-on-rectangle (width height)
  "Choose a random point on this rectangle:

   (0,0)                            (width, 0)
     +-------------------------------+
     |                               |
     |                               |
     |                               |
     +-------------------------------+
   (0, height)                      (width, height)
"
  (let ((horizontal? (coin-flip))       ; Do we choose a random edge
                                        ; on the top/bottom or
                                        ; left/right?

        (nearest? (coin-flip))          ; Do we choose the edge
                                        ; nearest to the top-left
                                        ; corner or furthest?

        (random-scale (random 1.0)))    ; Choose a random scale factor
                                        ; between 0 and 1 inclusive.
    (if horizontal?
        (list (* random-scale width)      ; Random X.
              (if nearest? 0 height))
        
        (list (if nearest? 0 width)
              (* random-scale height))))) ; Random Y.