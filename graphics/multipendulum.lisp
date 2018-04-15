;;;; multipendulum.lisp
;;;; Copyright (c) 2013 Robert Smith

#-capi (error "This code requires LispWorks CAPI.")

(defconstant gravity 9.8)
(defconstant 2pi (* 2 pi))

(defstruct pendulum
  initial-angle
  length)

;; A factor of 2*pi was taken out of the following since it gets
;; divided out in the end anyway.

(defun pendulum-frequency (pendulum)
  (/ (sqrt (/ (pendulum-length pendulum)
              gravity))))

(defun pendulum-angle (pendulum time)
  (* (pendulum-initial-angle pendulum)
     (sin (* time (pendulum-frequency pendulum)))))

(defun pendulum-position (pendulum time)
  (let ((angle (pendulum-angle pendulum time))
        (length (pendulum-length pendulum)))
    (cons
     (* length (sin angle))
     (* length (cos angle)))))

;;;; Drawing Code

(defparameter *pendulums* nil)

(defparameter *time* 0)

(defun plot-point (port point)
  (gp:draw-circle port (car point) (cdr point) 1 :filled t))

(defun add-pendulum (length initial-angle)
  (push (make-pendulum :initial-angle initial-angle
                       :length length)
        *pendulums*))

(defparameter *display-callback*
  (lambda (self x y width height)
    (declare (ignore x y width height))
    (gp:with-graphics-translation (self 400 10)
      (gp:with-graphics-scale (self 4 4)
        (mapc (lambda (pendulum)
                (let ((p (pendulum-position pendulum *time*)))
                  ;(gp:draw-line self 0 0 (car p) (cdr p) :foreground (color:make-gray 0.5 0.5))
                  (plot-point self p)))
              *pendulums*)))))

(capi:define-interface canvas-intf ()
  ((timer :initform nil
          :accessor canvas-timer))
  (:panes
   (canvas capi:output-pane
           :accessor canvas
           :display-callback *display-callback*
           :drawing-mode :quality
           :input-model `(((:button-1 :press)
                           ,(lambda (self x y)
                              (declare (ignore x y))
                              (gp:invalidate-rectangle self))))))
  (:layouts
   (main capi:row-layout '(canvas)))
  (:default-initargs :title "Multipendulum"
                     :width 800
                     :height 600))

(defun animate (intf)
  (let ((timer (mp:make-timer
                #'(lambda (intf)
                    (capi:execute-with-interface-if-alive
                     intf
                     'animate-step
                     intf
                     0.02))
                intf)))
    (setf (canvas-timer intf) timer)
    (mp:schedule-timer-relative timer 0.0)))

(defun animate-step (intf delay)
  (with-slots (timer) intf
    (capi:with-atomic-redisplay (intf)
      (setf *time* (+ 1/5 *time*)))
    (gp:invalidate-rectangle (canvas intf))
    (when timer
      (mp:schedule-timer-relative timer delay))))

(defvar *port*)

(defun new-canvas ()
  (setf *time* 0)
  (let* ((intf (capi:display (make-instance 'canvas-intf))))
    (setf *port* (canvas intf))
    (capi:execute-with-interface intf 'animate intf)
    intf))

;;;; test data

(defun sq (n) (* n n))

(defun lengths (index &key (duration 30) (oscillations 51))
  (* 1600
     gravity
     (sq (/ duration
            (* 2pi (+ index oscillations))))))

(defun initialize-pendulums (n)
  (setf *pendulums* nil)
  (let ((pi/3 (/ pi 4)))
    (dotimes (i n)
      (add-pendulum (lengths i) pi/3))))
