;;;; linear-track-pendulum.lisp
;;;; Copyright (c) 2013 Robert Smith

#-capi (error "This code requires LispWorks CAPI.")

(ql:quickload :gp-ext)

;;;;;;;;;;;;;;;;;;;;;;;; Modeling and Solving ;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant G  -9.8)
(defconstant fpi (coerce pi 'single-float))
(defconstant degrees (/ fpi 180.0))

;;; TODO: get rid of these in favor of static linear-tracked-pendulum state.
(defconstant L 1.0)
(defconstant M1 1.0)
(defconstant M2 1.0)

;;; TODO: Actually use this.
(defstruct linear-tracked-pendulum
  "The (static) state, or parameters, of the linear tracked pendulum system."
  ;; Length of the first rod to the first bob
  (length1 1.0 :type float)
  ;; Mass of the first bob
  (mass1   1.0 :type float)
  ;; Length of the rod from the first bob to the second.
  (length2 1.0 :type float)
  ;; Mass of the second bob.
  (mass2   1.0 :type float))

(defstruct dynamic-state
  "The time-dependent state of the linear tracked pendulum system."
  ;; The angle made between the first rod and the vertical.
  (theta1    0.0 :type float)
  ;; The angular momentum of the first bob.
  (momentum1 0.0 :type float)
  ;; The angle made between the second rod and the vertical.
  (displacement    0.0 :type float)
  ;; The angular momentum of the second bob.
  (horizontal-velocity 0.0 :type float))

(defun array-of-dynamic-state (dstate)
  (vector (dynamic-state-theta1 dstate)
          (dynamic-state-momentum1 dstate)
          (dynamic-state-displacement dstate)
          (dynamic-state-horizontal-velocity dstate)))

(declaim (inline sq))
(defun sq (x) (* x x))

(defun make-float-array (n)
  (make-array n :element-type 'float :initial-element 0.0))

;; [t w x v]

;;; TODO: Use static state of the system instead of constants.
(defun compute-derivatives (time y &optional dy/dt)
  "Compute the derivative at Y at time T, optionally putting the result in DY/DT, for the linear tracked pendulum system."
  (declare (ignore time))
  (let* ((dy/dt (or dy/dt (make-float-array (length y))))
         (Q (/ (* M2 L) (+ M1 M2))))
    
    (symbol-macrolet ((tt (aref y 0))
                      (ww (aref y 1))
                      (xx (aref y 2))
                      (vv (aref y 3)))
      (let ((tdd (/ (- (* G (sin tt))
                       (* Q
                          (sin tt)
                          (cos tt)
                          (sq ww)))
                    (- L
                       (* Q
                          (sin tt)
                          (sq (cos tt))
                          (sq ww))))))
        
        ;; t' = w
        (setf (aref dy/dt 0) ww)
        
        ;; w' = ...
        (setf (aref dy/dt 1)
              ;; t''
              tdd)
        
        ;; x' = v
        (setf (aref dy/dt 2) vv)
        
        ;; v' = ...
        (setf (aref dy/dt 3)
              ;; x''
              (* Q (- (* (sq ww)
                         (sin tt))
                      (* (cos tt)
                         tdd))))))
    
    ;; Return the derivative.
    dy/dt))

(defmacro each-into (result (&rest inputs) &body body)
  "For each of the sequences bound to the variables INPUTS, execute BODY as if the variables in INPUTS were bound to their elements. Put the results into RESULT."
  ;; Try to use a DOTIMES & SYMBOL-MACROLET-1?
  `(map-into ,result (lambda (,@inputs) ,@body) ,@inputs))

(defun rk4 (x y h diff &optional yout)
  "Perform multivariate Runge-Kutta given the x-value X, the y-vector Y, a step H, and a differential function DIFF. The function DIFF should have the lambda list

    (X Y &OPTIONAL DY/DX)

where DY/DX is an optional vector in which the result will be put."
  (let* ((num-equations (length y))
         (yout (or yout (make-float-array num-equations)))
         (xh (+ x (/ h 2)))
         (dy/dt (make-float-array num-equations))
         (y* (make-float-array num-equations))
         (k1 (make-float-array num-equations))
         (k2 (make-float-array num-equations))
         (k3 (make-float-array num-equations))
         (k4 (make-float-array num-equations)))
    (setf dy/dt (funcall diff x y dy/dt))
    (each-into k1 (dy/dt) (* h dy/dt))
    (each-into y* (y k1) (+ y (* 0.5 k1)))
    
    (setf dy/dt (funcall diff xh y* dy/dt))
    (each-into k2 (dy/dt) (* h dy/dt))
    (each-into y* (y k2) (+ y (* 0.5 k2)))
    
    (setf dy/dt (funcall diff xh y* dy/dt))
    (each-into k3 (dy/dt) (* h dy/dt))
    (each-into y* (y k3) (+ y k3))
    
    (setf dy/dt (funcall diff (+ x h) y* dy/dt))
    (each-into k4 (dy/dt) (* h dy/dt))
    (each-into yout (y k1 k2 k3 k4) (+ y
                                       (/ k1 6)
                                       (/ k2 3)
                                       (/ k3 3)
                                       (/ k4 6)))
    
    ;; Return result.
    yout))

(defstruct (evolution (:constructor %make-evolution))
  (step      0   :type unsigned-byte)
  (time      0.0 :type float)
  (time-step 0.0 :type float)
  (state     nil))

(defun make-evolution (&key (start-time 0.0)
                            (time-step (float 0.01))
                            (initial-theta1     90.0)
                            (initial-momentum1   0.0)
                            (initial-displacement    -10.0)
                            (initial-horizontal-velocity   0.0))
  "Make a new evolution object, which describes the dynamical system at a point in time."
  (%make-evolution :step 0
                   :time start-time
                   :time-step time-step
                   :state (make-dynamic-state
                           :theta1    (* initial-theta1 degrees)
                           :momentum1 (* initial-momentum1 degrees)
                           :displacement    (* initial-displacement degrees)
                           :horizontal-velocity (* initial-horizontal-velocity degrees))))

(defun evolve (ev)
  "Evolve the evolution EV by one time step."
  ;; Integrate system (solving for the next dynamic state vector).
  (let ((next (rk4 (evolution-time ev)
                   (array-of-dynamic-state (evolution-state ev))
                   (evolution-time-step ev)
                   #'compute-derivatives)))
    
    ;; Update the step and time.
    (incf (evolution-step ev))
    (incf (evolution-time ev) (evolution-time-step ev))
    
    ;; Update the dynamic state.
    (let ((dstate (evolution-state ev)))
      (setf (dynamic-state-theta1 dstate)    (aref next 0)
            (dynamic-state-momentum1 dstate) (aref next 1)
            (dynamic-state-displacement dstate)    (aref next 2)
            (dynamic-state-horizontal-velocity dstate) (aref next 3)))
    
    ;; Return the updated evolution.
    ev))


;;;;;;;;;;;;;;;;;;;;;;;;;;; Animation Code ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Queue Implementation (taken from stack-queue.lisp)

(defparameter *maximum-queue-size* 100)

(defstruct (queue (:constructor %make-queue)
                  (:predicate queuep))
  (elements nil :type list)
  (last nil :type (or null (cons t null)))
  (len 0 :type unsigned-byte))

(defun make-queue ()
  "Create a new empty queue."
  (%make-queue))

(defun queue-empty-p (queue)
  "Is the queue QUEUE empty?"
  (null (queue-elements queue)))

(defun list-to-queue (list)
  "Convert the list LIST into a queue. Note: LIST may be modified."
  (%make-queue :elements list
               :last (last list)))

(defun enqueue (queue obj)
  "Add an element OBJ to the end of the queue QUEUE."
  (when (> (incf (queue-len queue)) *maximum-queue-size*)
    (dequeue queue))
  (let ((last (list obj)))
    (if (queue-empty-p queue)
        ;; Set up the queue with the first element. Note that the same
        ;; reference to the singleton list is shared by both
        ;; QUEUE-ELEMENTS and QUEUE-LAST.
        (setf (queue-elements queue) last
              (queue-last queue)     last)
        
        ;; We can now append elements to QUEUE-ELEMENTS simply by
        ;; modifying QUEUE-LAST, whose reference is shared by
        ;; QUEUE-ELEMENTS,
        ;;
        ;; We do this instead of a single SETF for type safety of
        ;; QUEUE-LAST.
        (let ((old (queue-last queue)))
          (setf (queue-last queue) last
                (cdr old)          last))))
  queue)

(defun dequeue (queue)
  "Remove and return an element from the queue QUEUE."
  (decf (queue-len queue))
  (pop (queue-elements queue)))


;;;; Drawing and Display Code

(defvar *port* nil)

(defparameter *evolution* (make-evolution :time-step 0.01
                                          :initial-theta1 90.0
                                          :initial-momentum1 50.0
                                          :initial-displacement 0.0
                                          :initial-horizontal-velocity -5.0))

(defun draw-faded-polygon (port points)
  (loop :for p :on points :by #'cddr
        :for i :from 0.0 :by 1.5
        :for percent := (/ i *maximum-queue-size*)
        :do (when (third p)
              (gp:draw-line port
                            (second p)
                            (first p)
                            (fourth p)
                            (third p)
                            :foreground (color:make-rgb 0.0 0.0 1.0 percent)))))

(defparameter *display-callback*
  (lambda (self x y width height)
    (declare (ignore x y width height))
    
    (gp:with-graphics-translation (self 400 100)
      (let ((tt (dynamic-state-theta1 (evolution-state *evolution*)))
            (xx (dynamic-state-displacement (evolution-state *evolution*))))
        (let* ((theta (+ (/ fpi 2) tt))
               (pos1-x (* 100 xx))
               (pos1-y 0)
               (pos2-x (+ pos1-x (* 100 L (cos theta))))
               (pos2-y (+ pos1-y (* 100 L (sin theta)))))

          (with-slots (background-trace) self
            (enqueue background-trace pos2-y)
            (enqueue background-trace pos2-x)
            (draw-faded-polygon self (queue-elements background-trace)))
          
          #+ignore
          (gp:draw-line self
                        0 0
                        pos1-x
                        pos1-y)
          
          (let ((gp-ext::*arrowhead-width* (* 2 gp-ext::*arrowhead-width*))
                (gp-ext::*arrowhead-length* (* 2 gp-ext::*arrowhead-length*)))
            (gp-ext:draw-arrow self
                               0 0
                               pos1-x
                               pos1-y))
          (gp:draw-line self
                        pos1-x
                        pos1-y
                        pos2-x
                        pos2-y)
          
          (gp:draw-circle self
                          pos1-x
                          pos1-y
                          5
                          :filled t
                          :foreground :black)
          (gp:draw-circle self
                          pos2-x
                          pos2-y
                          5
                          :filled t
                          :foreground :black))))
    
    (lw:do-nothing)))

(defclass trace-pane (capi:output-pane)
  ((background-trace :initform (make-queue))))

(capi:define-interface canvas-intf ()
  ((timer :initform nil
          :accessor canvas-timer))
  (:panes
   (canvas trace-pane
           :accessor canvas
           :display-callback *display-callback*
           :drawing-mode :quality
           :input-model `(((:button-1 :press)
                           ,(lambda (self x y)
                              (declare (ignore x y))
                              (gp:invalidate-rectangle self))))))
  (:layouts
   (main capi:row-layout '(canvas)))
  (:default-initargs :title "Linear Tracked Pendulum"
                     :width 800
                     :height 600))

(defun animate (intf)
  (let ((timer (mp:make-timer
                #'(lambda (intf)
                    (capi:execute-with-interface-if-alive
                     intf
                     'animate-step
                     intf
                     0.01))
                intf)))
    (setf (canvas-timer intf) timer)
    (mp:schedule-timer-relative timer 0.0)))

(defun animate-step (intf delay)
  (with-slots (timer) intf
    (capi:with-atomic-redisplay (intf)
      (evolve *evolution*))
    (gp:invalidate-rectangle (canvas intf))
    (when timer
      (mp:schedule-timer-relative timer delay))))


;;;; Entry Point

(defun start-simulation ()
  (let* ((intf (capi:display (make-instance 'canvas-intf))))
    (setf *port* (canvas intf))
    (capi:execute-with-interface intf 'animate intf)
    intf))
