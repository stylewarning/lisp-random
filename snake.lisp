;;;; snake.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

;;;; An implementation of snake.

(ql:quickload :cl-charms)

(load "circular-queue.lisp")

(defstruct (point (:constructor make-point (x y)))
  x
  y)

(defun point= (a b)
  (and (= (point-x a) (point-x b))
       (= (point-y a) (point-y b))))

(defun draw-point (p &key (char #\*))
  (charms:with-restored-cursor charms:*standard-window*
    (charms:write-char-at-point charms:*standard-window*
                                char
                                (point-x p)
                                (point-y p))))

(defstruct snake
  direction
  queue)

(defun draw-snake (snake)
  (map-queue #'draw-point (snake-queue snake)))

(defun snake-head (snake)
  (copy-point (queue-back (snake-queue snake))))

(defun snake-point-p (snake point)
  (map-queue (lambda (snake-point)
               (when (point= point snake-point)
                 (return-from snake-point-p t)))
             (snake-queue snake))
  ;; Return NIL otherwise.
  nil)

(defun random-excluding-snake (snake width height)
  (loop :for point := (make-point (random width) (random height))
        :while (snake-point-p snake point)
        :finally (return point)))

(defparameter *tick* (* 0.15 internal-time-units-per-second))

(defun main ()
  "Start the timer program."
  (charms:with-curses ()
    (charms:disable-echoing)
    (charms:enable-raw-input :interpret-control-characters t)
    (charms:enable-non-blocking-mode charms:*standard-window*)
    (multiple-value-bind (width height)
        (charms:window-dimensions charms:*standard-window*)
      (let ((snake (make-snake :direction ':up
                               :queue (make-queue)))
            (proposed-next-direction :up)
            (cash-money nil)
            (grow nil))
        (labels ((normalize-point (p)
                   (make-point (mod (point-x p) width)
                               (mod (point-y p) height)))
                 (generate-cash-money ()
                   (setf cash-money (random-excluding-snake snake width height)))
                 (set-direction (dir)
                   (ecase dir
                     ((:up) (unless (eq :down (snake-direction snake))
                              (setf (snake-direction snake) dir)))
                     ((:down) (unless (eq :up (snake-direction snake))
                                (setf (snake-direction snake) dir)))
                     ((:left) (unless (eq :right (snake-direction snake))
                                (setf (snake-direction snake) dir)))
                     ((:right) (unless (eq :left (snake-direction snake))
                                 (setf (snake-direction snake) dir)))))
                 (point-in-direction (p dir)
                   (ecase dir
                     ((:up) (normalize-point (make-point (point-x p) (1- (point-y p)))))
                     ((:down) (normalize-point (make-point (point-x p) (1+ (point-y p)))))
                     ((:left) (normalize-point (make-point (1- (point-x p)) (point-y p))))
                     ((:right) (normalize-point (make-point (1+ (point-x p)) (point-y p))))))
                 (advance-snake (snake)
                   (let ((new-head (point-in-direction (snake-head snake)
                                                       (snake-direction snake))))
                     (if (snake-point-p snake new-head)
                         t
                         (progn
                           (queue-push new-head (snake-queue snake))
                           (if grow
                               (setf grow nil)
                               (queue-pop (snake-queue snake)))
                           nil)))))
          ;; Add the snake head.
          (queue-push (make-point (floor width 2) (floor height 2))
                      (snake-queue snake))
          
          ;; Set the initial cash.
          (generate-cash-money)
          
          (loop :named driver-loop
                :with time := (get-internal-real-time)
                :for c := (charms:get-char charms:*standard-window*
                                           :ignore-error t)
                :do (progn
                      ;; Clear the window and draw all entities.
                      (charms:clear-window charms:*standard-window* :force-repaint t)
                      (draw-point cash-money :char #\$)
                      (draw-snake snake)
                      (charms:refresh-window charms:*standard-window*)
                      
                      ;; Process input
                      (case c
                        ((nil) nil)
                        ((#\w) (setf proposed-next-direction :up))
                        ((#\a) (setf proposed-next-direction :left))
                        ((#\s) (setf proposed-next-direction :down))
                        ((#\d) (setf proposed-next-direction :right))
                        ((#\q #\Q) (return-from driver-loop)))
                      
                      ;; If enough time has passed, process it.
                      (when (< *tick* (- (get-internal-real-time) time))
                        ;; Set the direction and advance the
                        ;; snake. Return if we have collided.
                        (set-direction proposed-next-direction)
                        (when (advance-snake snake)
                          (return-from driver-loop 'dead))

                        ;; Check if we have collided with the money.
                        (when (point= cash-money (snake-head snake))
                          (setf grow t)
                          (generate-cash-money))
                        
                        (setf time (get-internal-real-time))))))))))
