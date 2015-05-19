;;;; rainbow.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

#-capi (error "This code requires LispWorks CAPI.")

(declaim (optimize speed (safety 0) (debug 0) (space 0)))

(defun hue (amount)
  (color:make-hsv (* amount 5.999)
                  1.0
                  1.0))

(defstruct speck
  position
  color
  velocity)

(defun speck-coordinates (speck &key pixel)
  (let ((x (realpart (speck-position speck)))
        (y (imagpart (speck-position speck))))
    (if pixel
        (values (round x) (round y))
        (values x y))))

(defun draw-speck (port speck)
  (multiple-value-bind (x y)
      (speck-coordinates speck :pixel t)
    (gp:draw-point port x y :foreground (speck-color speck))))

(defstruct (state (:constructor %make-state))
  (width 800)
  (height 600)
  ceiling
  (specks nil))

(defparameter *initial-speck*
  (make-speck :position 0
              :color (color:get-color-spec ':white)
              :velocity (/ (complex 1 1) (sqrt 2))))

(defun make-state (width height ceiling-height &key (initial-speck *initial-speck*))
  (assert (< ceiling-height height))
  (let ((ceiling (make-array (list width ceiling-height))))
    (dotimes (x width)
      (dotimes (y ceiling-height)
        (setf (aref ceiling x y) (hue (float (/ x (1- width)))))))
    (%make-state :width width
                 :height height
                 :ceiling ceiling
                 :specks (list initial-speck))))


(defparameter *dt* 1.0)

(defconstant pi/4 (coerce (/ pi 4) 'single-float))
(defconstant pi/3 (coerce (/ pi 3) 'single-float))
(defconstant pi/2 (coerce (/ pi 2) 'single-float))

(defun evolve (state)
  (let* ((width (state-width state))
         (height (state-height state))
         (ceiling (state-ceiling state))
         (ceiling-height (array-dimension ceiling 1)))
    (labels ((random-angle ()
               (let ((min (- (- pi/2) pi/3))
                     (max (+ (- pi/2) pi/3)))
                 (+ min (random (- max min)))))
             (evolve-speck (speck)
               (let* ((r (speck-position speck))
                      (v (speck-velocity speck))
                      (dr (* v *dt*))
                      (r+dr (+ r dr))
                      (x (realpart r+dr))
                      (y (imagpart r+dr))
                      (x-dot (realpart v))
                      (y-dot (imagpart v)))
                 
                 ;; Check left/right wall collision
                 (let ((undershoot? (minusp x))
                       (overshoot? (>= x width)))
                   (cond
                     (undershoot? (setf x (- x)))
                     (overshoot?  (setf x (- (* 2 width) x))))
                   (when (or undershoot? overshoot?)
                     (setf x-dot (- x-dot))))
                 
                 ;; Check top/bottom wall collision
                 (let ((undershoot? (minusp y))
                       (ceiling? (< (- height ceiling-height)
                                    y
                                    height))
                       (overshoot? (>= y height))
                       (ceiling-collision? nil))
                   (cond
                     (undershoot? (setf y (- y)))
                     (overshoot?  (setf y (- (* 2 height) y)))
                     (ceiling?
                      (let* ((ceiling-x (min (1- width) (round x)))
                             (ceiling-y (min (1- ceiling-height) (- ceiling-height (- height (round y)))))
                             (ceiling-pixel (aref ceiling ceiling-x ceiling-y)))
                        (when ceiling-pixel
                          (push (make-speck :position (complex x y)
                                            :velocity (cis (random-angle))
                                            :color ceiling-pixel)
                                (state-specks state))
                          (setf (aref ceiling ceiling-x ceiling-y) nil)
                          (setf ceiling-collision? t)))))
                   (when (or undershoot? overshoot? ceiling-collision?)
                     (setf y-dot (- y-dot))))
                 
                 (setf (speck-position speck) (complex x y)
                       (speck-velocity speck) (complex x-dot y-dot))
                 speck)))
      (mapc #'evolve-speck (state-specks state)))))


(defvar *port*)

(defvar *state* nil)

(defparameter *display-callback*
  (lambda (port x y width height)
    (declare (ignore x y width height))
    (mapc (lambda (speck)
            (draw-speck port speck))
          (state-specks *state*))

    (let* ((width (state-width *state*))
           (height (state-height *state*))
           (ceiling (state-ceiling *state*))
           (ceiling-height (array-dimension ceiling 1)))
      (loop :with offset := (- height ceiling-height)
            :for x :below width
            :do (loop :for y :below ceiling-height
                      :for color := (aref ceiling x y)
                      :when color
                        :do (gp:draw-point port
                                           x
                                           (+ y (- height ceiling-height))
                                           :foreground color))))))

(capi:define-interface canvas-intf ()
  ((timer :initform nil
          :accessor canvas-timer))
  (:panes
   (canvas capi:output-pane
           :accessor canvas
           :display-callback *display-callback*
           :drawing-mode :quality
           :background :black))
  (:layouts
   (main capi:row-layout '(canvas)))
  (:default-initargs :title "Rainbow"
                     :width 200
                     :height 150))

(defun animate (intf)
  (let ((timer (mp:make-timer
                #'(lambda (intf)
                    (capi:execute-with-interface-if-alive
                     intf
                     'animate-step
                     intf
                     0.001))
                intf)))
    (setf (canvas-timer intf) timer)
    (mp:schedule-timer-relative timer 0.0)))

(defun animate-step (intf delay)
  (with-slots (timer) intf
    (capi:with-atomic-redisplay (intf)
      (evolve *state*))
    (gp:invalidate-rectangle (canvas intf))
    (when timer
      (mp:schedule-timer-relative timer delay))))

(defun new-canvas ()
  (setf *state* (make-state 200 150 50))
  (let* ((intf (make-instance 'canvas-intf)))
    (capi:display intf)
    (setf *port* (canvas intf))
    (capi:execute-with-interface intf 'animate intf)
    intf))
