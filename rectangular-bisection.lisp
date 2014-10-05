(defun emptyp (ulx uly drx dry)
  "Does the rectangle with upper-left corner (ULX, ULY) and
lower-right corner (DRX, DRY) form an empty rectangle?"
  (or (not (plusp (- drx ulx)))
      (not (plusp (- dry uly)))))

(defun unitp (ulx uly drx dry)
  "Does the rectangle with upper-left corner (ULX, ULY) and
lower-right corner (DRX, DRY) form a rectangle with unit area?"
  (and (= 1 (- drx ulx))
       (= 1 (- dry uly))))

(defun horizontalp (ulx uly drx dry)
  "Does the rectangle with upper-left corner (ULX, ULY) and
lower-right corner (DRX, DRY) form a horizontal strip of height 1?"
  (and (= 1 (- dry uly))
       (plusp (- drx ulx))))

(defun verticalp (ulx uly drx dry)
    "Does the rectangle with upper-left corner (ULX, ULY) and
lower-right corner (DRX, DRY) form a vertical strip of width 1?"
  (and (= 1 (- drx ulx))
       (plusp (- dry uly))))

(defun middle (a b)
  "Compute the (approximate) middle of the integers A and B."
  (values (floor (+ a b) 2)))

(defun map-bisected-indexes (f height width)
  "Given a binary function F and a rectangle of height HEIGHT and
  width WIDTH, call the function F on all zero-indexed (row, column)
  coordinates of the rectangle in \"bisection order\"."
  (labels ((bisect-horizontal (depth xmin xmax y)
             "Bisect a horizontal rectangle via three subrectangles."
             (let ((middle (middle xmin xmax))
                   (depth+1 (1+ depth)))
               ;; Middle point
               (bisect-rectangle depth+1
                                 middle y
                                 (1+ middle) (1+ y))

               ;; Left rectangle
               (bisect-rectangle depth+1
                                 xmin y
                                 middle (1+ y))

               ;; Right rectangle
               (bisect-rectangle depth+1
                                 (1+ middle) y
                                 xmax (1+ y))))

           (bisect-vertical (depth ymin ymax x)
             "Bisect a vertical rectangle via three subrectangles."
             (let ((middle (middle ymin ymax))
                   (depth+1 (1+ depth)))
               ;; Middle point
               (bisect-rectangle depth+1
                                 x middle
                                 (1+ x) (1+ middle))

               ;; Top rectangle
               (bisect-rectangle depth+1
                                 x ymin
                                 (1+ x) middle)

               ;; Bottom rectangle
               (bisect-rectangle depth+1
                                 x (1+ middle)
                                 (1+ x) ymax)))

           (bisect-rectangle (depth ulx uly drx dry)
             "Bisect a rectangle, ensuring the depth specified by the
              special variable *DEPTH-LIMIT* isn't exceeded."
             (declare (special *depth-limit*))
             (when (<= depth *depth-limit*)
               (perform-bisection depth ulx uly drx dry)))

           (perform-bisection (depth ulx uly drx dry)
             "Perform a 9-way bisection on the rectangle specified by
              the upper-left corner (ULX, ULY) and
              lower-right (exclusive) corner (DRX, DRY)."
             (declare (special *depth-limit*))
             (cond
               ;; Empty rectangle: do nothing
               ((emptyp ulx uly drx dry)
                nil)

               ;; Unit rectangle: test it
               ((unitp ulx uly drx dry)
                (when (= depth *depth-limit*)
                  (funcall f uly ulx)))

               ;; Horizontal rectangle: do linear bisection
               ((horizontalp ulx uly drx dry)
                (bisect-horizontal depth ulx drx uly))

               ;; Vertical rectangle: do linear bisection
               ((verticalp ulx uly drx dry)
                (bisect-vertical depth uly dry ulx))

               ;; General rectangle. Split it up into 9 subrectangles.
               (t
                (let ((cx (middle ulx drx))
                      (cy (middle uly dry))
                      (depth+1 (1+ depth)))
                  ;; Center Point

                  ;; middle
                  ;; (cx cy) -- (cx+1 cy+1)
                  (bisect-rectangle depth+1
                                    cx cy
                                    (1+ cx) (1+ cy))

                  ;; Linear Rectangles

                  ;; top
                  ;; (cx uly) -- (cx+1 cy)
                  (bisect-rectangle depth+1
                                    cx uly
                                    (1+ cx) cy)

                  ;; left
                  ;; (ulx cy) -- (cx cy+1)
                  (bisect-rectangle depth+1
                                    ulx cy
                                    cx (1+ cy))

                  ;; right
                  ;; (cx+1 cy) -- (drx cy+1)
                  (bisect-rectangle depth+1
                                    (1+ cx) cy
                                    drx (1+ cy))

                  ;; down
                  ;; (cx cy+1) -- (cx+1 dry)
                  (bisect-rectangle depth+1
                                    cx (1+ cy)
                                    (1+ cx) dry)

                  ;; Planar Rectangles

                  ;; top-left
                  ;; (ulx uly) -- (cx cy)
                  (bisect-rectangle depth+1
                                    ulx uly
                                    cx cy)


                  ;; top-right
                  ;; (cx+1 uly) -- (drx cy)
                  (bisect-rectangle depth+1
                                    (1+ cx) uly
                                    drx cy)

                  ;; down-left
                  ;; (ulx cy+1) -- (cx dry)
                  (bisect-rectangle depth+1
                                    ulx (1+ cy)
                                    cx dry)

                  ;; down-right
                  ;; (cx+1 cy+1) -- (drx dry)
                  (bisect-rectangle depth+1
                                    (1+ cx) (1+ cy)
                                    drx dry))))))
    (let ((max-depth (1+ (ceiling (log (max width height) 2)))))
      ;; Perform iterative-deepening depth-first search.
      (dotimes (depth max-depth)
        (let ((*depth-limit* depth))
          (declare (special *depth-limit*))
          (bisect-rectangle 0 0 0 width height))))))

;;; Test Code

(defun pprint-array (a)
  (destructuring-bind (height width) (array-dimensions a)
    (dotimes (y height)
      (dotimes (x width)
        (format t "~D " (aref a y x)))
      (terpri))))
