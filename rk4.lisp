;;;; rk4.lisp
;;;; Copyright (c) 2013 Robert Smith

;;;; A simple and minimal implementation of the classical Runge-Kutta
;;;; algorithm for solving IVPs.

(defun rk4-scanner (f t0 y0 h steps)
  "Approximate the solution to the differential equation:

    dy
    -- = f(t, y),    f(t0) = y0
    dt

using the classical Runge-Kutta method. The parameter H determines the
step size used (smaller is better), and the parameter STEPS determines
the number of points to approximate.

Return a high-order function which takes a binary function
SCANNING-FUNCTION which will be called with t_n and successive
approximations of y(t_n) where

    t_n = t0 + n*h.

The high-order function itself will return the last point approximated
as two values.

The error of each step is O(h^5) while the global error is O(h^4)."
  (check-type steps (integer 1)
              "The number of steps must be a positive integer.")
  (let ((h/2 (/ h 2))
        (tn t0)
        (yn y0))
    (lambda (scanning-function)
      (funcall scanning-function t0 y0)
      (dotimes (step steps)
        (let* ((t+h/2 (+ tn h/2))
               (k1 (funcall f tn       yn))
               (k2 (funcall f t+h/2    (+ yn (* k1 h/2))))
               (k3 (funcall f t+h/2    (+ yn (* k2 h/2))))
               (k4 (funcall f (+ tn h) (+ yn (* k3 h)))))
          
          ;; y_(n+1) = y_n + h * (k1 + 2k2 + 2k3 + k4)/6
          (incf yn (/ (* h (+ k1 k4 (* 2 (+ k2 k3))))
                      6))
          
          ;; t_(n+1) = t_n + h
          (incf tn h)
          
          (funcall scanning-function tn yn)))

      ;; Return the last values computed and reset the state so that
      ;; the function may be used again.
      (multiple-value-prog1 (values tn yn)
        (setf tn t0
              yn y0)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Example ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Suppose we have the IVP
;;;
;;;     dy
;;;     -- = -2y + x + 4,   y(0) = 1.
;;;     dx
;;;
;;; Then f(x, y) = -2y + x + 4. Note that the exact solution to this
;;; differential equation is
;;;
;;;     y(x) = (-3/4)exp(-2x) + x/2 + 7/4.

(defun exact-solution (x)
  (+ (* -3/4 (exp (* -2 x)))
     (/ x 2)
     7/4))

;;; Suppose we want to list the solutions along with the absolute
;;; errors. We will create a scanning function to do so.

(defun print-solutions (x y)
  (format t "~,1F: ~,5F  (~E)~%" x y (- y (exact-solution x))))

;;; Now we create a solution scanner. Let's approximate the solution
;;; for the values of x across the unit interval spaced out by 0.2.

(defvar *solver* (rk4-scanner (lambda (x y) (+ (* -2 y) x 4)) ; f(x, y)
                              0.0                             ; x0
                              1.0                             ; y0
                              0.2                             ; step size
                              5))                             ; step count

;;; Now we can print the solutions by calling the solver with our scanner:
;;;
;;; CL-USER> (funcall *solver* #'print-solutions)
;;; 0.0: 1.00000  (0.0E+0)
;;; 0.2: 1.34720  (-5.9962273E-5)
;;; 0.4: 1.61292  (-8.034706E-5)
;;; 0.6: 1.82402  (-8.08239E-5)
;;; 0.8: 1.99851  (-7.224083E-5)
;;; 1.0: 2.14844  (-6.055832E-5)
;;; 1.0
;;; 2.148438
;;;
;;; Note that the last two values are the last values computed by the
;;; solver. This is useful if we only want to compute our solution at
;;; a point:

(defun ignore-point (x y)
  (declare (ignore x y))
  (values))

;;; So now we can simply do the following without doing extra
;;; intermediate computations:
;;;
;;; CL-USER> (funcall *solver* #'ignore-point)
;;; 1.0
;;; 2.148438
