;;;; area-pi.lisp
;;;; Copyright (c) 2018 Robert Smith

(ql:quickload :lparallel)

;;; In all of the below challenges, you may *not* use floating point
;;; numbers.

;;; #1: Suppose we have a square in the first quadrant, and a quarter
;;; circle inscribed in this square. Compute the ratio of the area of
;;; the quarter circle to the area of the square.

(defparameter *default-resolution* (isqrt most-positive-fixnum))
(defparameter *default-digits* 10)

(defun compute-ratio (num-points &key (resolution *default-resolution*))
  ;; resolution = number of points per unit
  (declare (optimize speed (safety 0) (debug 0) (space 0)))
  (check-type resolution (integer 1 #.*default-resolution*))
  (check-type num-points fixnum)
  (let ((r^2 (* resolution resolution))
        (x (random resolution))
        (y (random resolution)))
    (check-type r^2 (integer 1 #.(expt *default-resolution* 2)))
    (loop :repeat num-points
          :do (setf x (random resolution)
                    y (random resolution))
          :count (<= (+ (* x x) (* y y)) r^2))))

;;; #2: The above ratio is a representation of the probability a
;;; random point lands in the circle. Convince yourself that this
;;; probability is pi/4, and use this fact to produce a ratio that
;;; approximates pi.

(defun pi-ratio (num-points &key (resolution *default-resolution*))
  (let ((r (compute-ratio num-points :resolution resolution)))
    (values (* 4 r)
            num-points)))

;;; #3: Print out the digits of pi using the above
;;; approximation. Again, you're not allowed to use floating point
;;; numbers.

(defun long-divide (num den &key (digits *default-digits*))
  (assert (plusp den))
  (labels ((%long-divide (num digits)
             (cond
               ((or (zerop num)
                    (zerop digits))
                (terpri)
                nil)
               ((< num den)
                (%long-divide (* 10 num) digits))
               (t
                (multiple-value-bind (quo rem) (floor num den)
                  (format t "~D " quo)
                  (%long-divide rem (1- digits)))))))
    (%long-divide num digits)))

(defun print-pi-digits (num-points &key (resolution *default-resolution*)
                                        (digits *default-digits*))
  (multiple-value-bind (num den)
      (pi-ratio num-points :resolution resolution)
    (long-divide num den :digits digits)))
