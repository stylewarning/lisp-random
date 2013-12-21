;;;; delaunay.lisp
;;;; Copyright (c) 2013 Robert Smith

;;;; This file contains code for computing the Delaunay triangulation
;;;; of a set of points.

#-capi (error "This code requires LispWorks CAPI.")

;;;; Utilities

;;; This is really slow.
(defun uniques (list &key (test #'eql))
  "Extract the unique items of the list LIST according to the equality
test TEST."
  (loop :for x :in list
        :when (= 1 (count x list :test test))
          :collect x))

(defun float-zerop (x &optional (eps (* 2 single-float-epsilon)))
  "Is X equal to zero (within tolerance)?"
  (< (abs x) eps))


;;;; Points

(defstruct (point (:constructor make-point (x y)))
  (x 0.0 :type float)
  (y 0.0 :type float))

(defmacro with-point ((x y) p &body body)
  "Destructure a point P into its component Cartesian coordinates X
and Y."
  (let ((p-once (gensym "P-ONCE-")))
    `(let* ((,p-once ,p)
            (,x (point-x ,p-once))
            (,y (point-y ,p-once)))
       ,@body)))

(defun point-distance (p &key square)
  "Compute the distance between the point P and the origin. If SQUARE
is true, return the square of the distance."
  (with-point (x y) p
    (let ((sq-dist (+ (* x x) (* y y))))
      (if square
          sq-dist
          (sqrt sq-dist)))))

(defun point-difference (p q)
  "Compute the difference between the points P and Q."
  (with-point (px py) p
    (with-point (qx qy) q
      (make-point (- px qx) (- py qy)))))

(defun point= (p q)
  "Are the points P and Q the same (within tolerance)?"
  (let ((delta (point-difference p q)))
    (and (float-zerop (point-x delta))
         (float-zerop (point-y delta)))))

(defun random-point (&key (max-x 640.0) (max-y 480.0))
  "Generate a random point."
  (make-point (+ 20 (random max-x))
              (+ 20 (random max-y))))


;;;; Circles

(defstruct circle
  (center (make-point 0.0 0.0) :type point)
  (radius 0.0 :type float))

(defun point-in-circle-p (point circle)
  "Is the point POINT in the circle CIRCLE?"
  (< (point-distance (point-difference point (circle-center circle)))
     (circle-radius circle)))


;;;; Triangles

(defstruct (triangle (:constructor %make-triangle))
  vertices
  circumcircle)

(defmacro with-vertices ((a b c) tri &body body)
  "Destructure a triangle TRI into its component vertices A, B, and C."
  (let ((verts (gensym "VERTS-")))
    `(let* ((,verts (triangle-vertices ,tri))
            (,a (aref ,verts 0))
            (,b (aref ,verts 1))
            (,c (aref ,verts 2)))
       ,@body)))

(defun circumcircle (pa pb pc)
  "Compute the circumcircle of a triangle with vertices PA, PB, and PC."
  (flet ((radius (pa pb pc)
           (let* ((a (point-distance (point-difference pb pc)))
                  (b (point-distance (point-difference pc pa)))
                  (c (point-distance (point-difference pa pb)))
                  (denom (sqrt
                          (* (+   a     b     c)
                             (+ (- a)   b     c)
                             (+   a   (- b)   c)
                             (+   a     b   (- c))))))
             (if (float-zerop denom)
                 -1.0
                 (/ (* a b c) denom))))
         (center (pa pb pc)
           (with-point (ax ay) pa
             (with-point (bx by) pb
               (with-point (cx cy) pc
                 (let ((a (point-distance pa :square t))
                       (b (point-distance pb :square t))
                       (c (point-distance pc :square t))
                       (by-cy (- by cy))
                       (cy-ay (- cy ay))
                       (ay-by (- ay by)))
                   (let ((ux (+ (* a by-cy)
                                (* b cy-ay)
                                (* c ay-by)))
                         (uy (+ (* a (- cx bx))
                                (* b (- ax cx))
                                (* c (- bx ax))))
                         (d (+ (* 2 ax by-cy)
                               (* 2 bx cy-ay)
                               (* 2 cx ay-by))))
                     (make-point (/ ux d)
                                 (/ uy d)))))))))
    (let ((r (radius pa pb pc)))
      ;; TODO, deal with colinear points properly (r < 0).
      (if (plusp r)
          (make-circle
           :center (center pa pb pc)
           :radius r)
          (make-circle
           :center (make-point 0.0 0.0)
           :radius 0.0)))))

(defun make-triangle (pa pb pc)
  "Make a triangle with vertices PA, PB, and PC."
  (%make-triangle :vertices (vector pa pb pc)
                  :circumcircle (circumcircle pa pb pc)))

(defun point-in-circumcircle-p (point triangle)
  "Is the point POINT in the circumcircle of the triangle TRIANGLE?"
  (point-in-circle-p point (triangle-circumcircle triangle)))

(defun has-coinciding-vertices-p (a b)
  "Do the triangles A and B have coinciding vertices?"
  (with-vertices (a1 a2 a3) a
    (with-vertices (b1 b2 b3) b
      (or (point= a1 b1) (point= a1 b2) (point= a1 b3)
          (point= a2 b1) (point= a2 b2) (point= a2 b3)
          (point= a3 b1) (point= a3 b2) (point= a3 b3)))))


;;;; Edges

(defstruct (edge (:constructor %make-edge))
  initial
  terminal)

;;; FIXME: normalize for equal x and unequal y.
(defun make-edge (initial terminal)
  "Make an edge with the initial and terminal points INITIAL and
TERMINAL. The edge will be normalized so the x-coordinates are
ascending."
  (if (<= (point-x initial) (point-x terminal))
      (%make-edge :initial initial
                  :terminal terminal)
      (%make-edge :initial terminal
                  :terminal initial)))

(defun edge= (a b)
  "Are two edges the same (within tolerance)?"
  (and (point= (edge-initial a) (edge-initial b))
       (point= (edge-terminal a) (edge-terminal b))))

(defun triangle-edges (triangle)
  "Return a list of the edges of the triangle TRIANGLE."
  (with-vertices (a b c) triangle
    (list (make-edge a b)
          (make-edge b c)
          (make-edge c a))))


;;;; Triangulation

(defstruct triangulation
  points
  supertriangle
  triangles)

(defun bounding-box (points)
  "Compute the minimum bounding box containing the points
POINTS. Return four values: the minimum x and y coordinates, and the
maximum x and y coordinates, respectively."
  (loop :for p :in points
        :for x := (point-x p)
        :for y := (point-y p)
        :minimize x :into min-x
        :minimize y :into min-y
        :maximize x :into max-x
        :maximize y :into max-y
        :finally (return (values min-x min-y
                                 max-x max-y))))

;;; XXX FIXME with proper bounds. If the supertriangle is too small,
;;; then we don't get a convex hull. If it is massive, then our
;;; circumcircles will be large and could potentially cause floating
;;; point overflow.
(defun triangle-containing (a b p q &optional (offset 10.0))
  "Make a triangle that contains the rectangle whose min coordinate
  is (a,b) and max coordinate is (p,q)."
  (declare (ignore a b p q offset))
  (make-triangle (make-point -1000.0 -1000.0)
                 (make-point -1000.0  5000.0)
                 (make-point  5000.0 -1000.0)))

(defun supertriangle (points)
  "Compute a supertriangle containing the points POINTS."
  (multiple-value-bind (min-x min-y max-x max-y) (bounding-box points)
    (triangle-containing min-x min-y max-x max-y)))

(defun add-point (point triangulation)
  "Add the point POINT to the triangulation TRIANGULATION and
retriangulate as necessary."
  (flet ((%add-point (point triangles)
           (let ((legal-triangles nil)
                 (edges nil))
             ;; Find the illegal triangles and collect their edges.
             (dolist (triangle triangles)
               (if (point-in-circumcircle-p point triangle)
                   (setf edges (nreconc (triangle-edges triangle) edges))
                   (push triangle legal-triangles)))
             
             ;; Construct legal triangles and return all of them.
             (dolist (edge (uniques edges :test #'edge=) legal-triangles)
               (push (make-triangle point
                                    (edge-initial edge)
                                    (edge-terminal edge))
                     legal-triangles)))))
    
    ;; Add point and fix triangles.
    (push point (triangulation-points triangulation))
    (setf (triangulation-triangles triangulation)
          (%add-point point (triangulation-triangles triangulation)))
    
    ;; Return the updated triangulation.
    triangulation))

(defun retriangulate (triangulation)
  "Retriangulate the triangulation TRIANGULATION solely based off of
its points. This will recompute the supertriangle as well as the
triangulation."
  (let* ((supertriangle (supertriangle (triangulation-points triangulation)))
         (vertices (triangle-vertices supertriangle))
         (points (list* (aref vertices 0)
                        (aref vertices 1)
                        (aref vertices 2)
                        (triangulation-points triangulation))))
    
    ;; Reset the supertriangle and add it to the triangle list.
    (setf (triangulation-supertriangle triangulation)
          supertriangle)
    
    (setf (triangulation-triangles triangulation)
          (list supertriangle))
    
    ;; Add each point iteratively and return the triangulation.
    (dolist (point points triangulation)
      (add-point point triangulation))))

(defun triangulate (points)
  "Compute a triangulation for the points POINTS."
  (retriangulate (make-triangulation :points points)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Graphics ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; The graphics code require's LispWorks' CAPI.

(defun draw-point (port point)
  (gp:draw-circle port
                  (point-x point)
                  (point-y point)
                  3
                  :filled t
                  :foreground :black))

(defun draw-circle (port circle)
  (gp:draw-circle port
                  (point-x (circle-center circle))
                  (point-y (circle-center circle))
                  (circle-radius circle)
                  :foreground :skyblue))

(defun draw-triangle (port triangle &optional (color :black))
  (let ((a (aref (triangle-vertices triangle) 0))
        (b (aref (triangle-vertices triangle) 1))
        (c (aref (triangle-vertices triangle) 2)))
    (gp:draw-line port
                  (point-x a) (point-y a)
                  (point-x b) (point-y b)
                  :foreground color)
    
    (gp:draw-line port
                  (point-x b) (point-y b)
                  (point-x c) (point-y c)
                  :foreground color)
    
    (gp:draw-line port
                  (point-x c) (point-y c)
                  (point-x a) (point-y a)
                  :foreground color)))
  

(defvar *port* nil)

(defvar *triangulation* nil)

(defparameter *display-callback*
  (lambda (self x y width height)
    (declare (ignore x y width height))
    
    ;; Draw the circumcircles.
    (mapc (lambda (triangle)
            (unless (has-coinciding-vertices-p
                     triangle
                     (triangulation-supertriangle *triangulation*))
              (draw-circle self (triangle-circumcircle triangle))))
          (triangulation-triangles *triangulation*))
    
    ;; Draw the triangles.
    (mapc (lambda (triangle)
            (unless (has-coinciding-vertices-p
                     triangle
                     (triangulation-supertriangle *triangulation*))
              (draw-triangle self triangle)))
          (triangulation-triangles *triangulation*))
    
    ;; Draw the points.
    (mapc (lambda (point)
            (draw-point self point))
          (triangulation-points *triangulation*))
    
    (lw:do-nothing)))

(capi:define-interface canvas-intf () ()
  (:panes
   (canvas capi:output-pane
           :accessor canvas
           :display-callback *display-callback*
           :drawing-mode :quality
           :input-model `(
                         ((:button-1 :press)
                          ,(lambda (self x y)
                             (add-point (make-point x y) *triangulation*)
                             (gp:invalidate-rectangle self))))))
  (:layouts
   (main capi:row-layout
         '(canvas)))
  (:default-initargs :title "Triangulation"
                     :width 800
                     :height 600))

;;; XXX FIXME: Fails with this set of points.
(defun circular-points ()
  (loop :for theta :from 0.0 :to (* 2 pi) :by (/ pi 4)
        :collect (make-point (+ 320 (* 100 (cos theta)))
                             (+ 240 (* 100 (sin theta))))))

(defun new-canvas ()
  (setf *triangulation* (triangulate nil))
  (let* ((intf (make-instance 'canvas-intf)))
    (capi:display intf)
    (setf *port* (canvas intf))
    intf))
