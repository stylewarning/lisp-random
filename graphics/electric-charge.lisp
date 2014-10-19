;;;; electric-charge.lisp
;;;; Copyright (c) 2013 Robert Smith

(defun sq (x) (* x x))

(defstruct (point (:constructor make-point (x y)))
  (x 0.0 :type float)
  (y 0.0 :type float))

(defun distance (p)
  "The distance between P and the origin."
  (sqrt (+ (sq (point-x p))
           (sq (point-y p)))))

(defun distance-cubed (p)
  "The cube of the distance between P and the origin."
  (expt (+ (sq (point-x p))
           (sq (point-y p)))
        3/2))

(defun sum (p1 p2)
  "Compute P1 + P2."
  (make-point (+ (point-x p1)
                 (point-x p2))
              (+ (point-y p1)
                 (point-y p2))))

(defun difference (p1 p2)
  "Compute P1 - P2."
  (make-point (- (point-x p1)
                 (point-x p2))
              (- (point-y p1)
                 (point-y p2))))

(defun scale (k p)
  "Compute k*p."
  (make-point (* k (point-x p))
              (* k (point-y p))))

(defun scale-add! (p k q)
  "Compute p := p + k*q."
  (setf (point-x p) (+ (point-x p)
                       (* k (point-x q)))
        (point-y p) (+ (point-y p)
                       (* k (point-y q)))))

(defun unit-angle (angle &optional (scale 1.0))
  "Return a unit vector in the direction ANGLE. Optionally scale it by
SCALE."
  (make-point (* scale (cos angle))
              (* scale (sin angle))))

(defun unit! (point)
  "Rewrite POINT so that it falls on the unit circle."
  (let ((dist (distance point)))
    (setf (point-x point) (/ (point-x point) dist)
          (point-y point) (/ (point-y point) dist))))


(defstruct charge
  (position (make-point 0.0 0.0) :type point)
  (magnitude 1.0                 :type float))

(defstruct charge-system
  (charges #() :type (vector charge)))

(defparameter +scale-factor+ 1.0) ;; (float (* 4 pi) 1.0)

(defun force-at (point sys)
  "Find the force exerted by the electrostatic force imposed by the
charge system SYS at the point POINT. The test charge is chosen so
that Coulomb's constant gets canceled out."
  (let ((fx 0.0)
        (fy 0.0))
    (flet ((contribute-charge (charge)
             (let* ((delta (difference point (charge-position charge)))
                    (coeff (/ (charge-magnitude charge)
                              (distance-cubed delta))))
               (incf fx (* coeff (point-x delta)))
               (incf fy (* coeff (point-y delta))))))
      (map nil #'contribute-charge (charge-system-charges sys))
      
      ;; Return the force vector
      (make-point (/ fx +scale-factor+)
                  (/ fy +scale-factor+)))))

(defun scan-trajectory (f point sys &key (h 0.1))
  "Scan an approximation of the trajectory of a charged particle in
the charge system SYS starting at point POINT with the binary function
F. F will receive the x and y coordinate of the point being
scanned. The step size can be controlled by H."
  (let ((p (copy-point point)))
    (loop
      (scale-add! p h (force-at p sys))
      (funcall f (point-x p) (point-y p)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TESTING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *sys*
  (make-charge-system
   :charges (vector (make-charge :magnitude 100.0
                                 :position (make-point 100.0 100.0))
                    (make-charge :magnitude 150.0
                                 :position (make-point 400.0 150.0))
                    (make-charge :magnitude -100.0
                                 :position (make-point 250.0 250.0))
                    (make-charge :magnitude 100.0
                                 :position (make-point 230.0 320.0)))))

(defparameter *field-lines* 16)

(defvar *port*)

(defun draw-charge (port charge)
  (gp:draw-circle port
                  (point-x (charge-position charge))
                  (point-y (charge-position charge))
                  10
                  :filled t
                  :foreground (if (plusp (charge-magnitude charge))
                                  :red
                                  :black)))

(defun draw-trajectory (port point sys)
  (let ((last-x (point-x point))
        (last-y (point-y point))
        (steps 0))
    (block nil
      (labels ((continue? (x y)
                 (and (<= 0.0 x 640.0)
                      (<= 0.0 y 480.0)
                      (<= 10.0 (sqrt
                                (+ (sq (- x 250.0))
                                   (sq (- y 250.0)))))
                      (< steps 100000)))
               
               (draw-segment (x y)
                 (gp:draw-line port last-x last-y x y)
                 (unless (continue? x y)
                   (return nil))
                 (setf last-x x
                       last-y y
                       steps (1+ steps))))
        (scan-trajectory #'draw-segment point sys :h 10)
        (lw:do-nothing)))))

(defparameter *display-callback*
  (lambda (self x y width height)
    (declare (ignore x y width height))

    ;; Draw the field lines.
    (map nil
         #'(lambda (charge)
             (when (plusp (charge-magnitude charge))
               (dotimes (i *field-lines*)
                 (let ((angle (float (/ (* 2 i pi) *field-lines*) 1.0)))
                   (draw-trajectory self
                                    (sum (charge-position charge)
                                         (unit-angle angle 10))
                                    *sys*)))))
         (charge-system-charges *sys*))
    
    ;; Draw the charges.
    (map nil
         #'(lambda (charge)
             (draw-charge self charge))
         (charge-system-charges *sys*))
    
    (lw:do-nothing)))

(capi:define-interface canvas-intf () ()
  (:panes
   (canvas capi:output-pane
           :accessor canvas
           :display-callback *display-callback*
           :drawing-mode :quality))
  (:layouts
   (main capi:row-layout
         '(canvas)))
  (:default-initargs :title "Electric Field"
                     :width 640
                     :height 480))


(defun new-canvas ()
  (let* ((intf (make-instance 'canvas-intf)))
    (capi:display intf)
    (setf *port* (canvas intf))
    intf))
