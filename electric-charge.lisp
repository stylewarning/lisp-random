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

(defvar +scale-factor+ (float (* 4 pi) 1.0))

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

(defun trajectory (point sys &optional (steps 10) (h 0.1))
  (let ((p (copy-point point)))
    (format t "start: (~A, ~A)~%" (point-x p) (point-y p))
    (dotimes (i steps)
      (scale-add! p h (force-at p sys))
      (format t "~D: (~A, ~A)~%" (1+ i) (point-x p) (point-y p)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TESTING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *sys*
  (make-charge-system
   :charges (vector (make-charge :magnitude 1.0)
                    (make-charge :magnitude -1.0
                                 :position (make-point 1.0 1.0)))))
