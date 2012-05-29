;;;; restarts.lisp
;;;; Copyright (c) 2012 Robert Smith

;;;; Accompanies the post http://symbo1ics.com/blog/?p=1405

;;;;;;;;;;;;;;;;;;;;;;;;;;;; LIBRARY CODE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun slope (p q)
  "Compute the slope/gradient of the line connecting points P and Q."
  ;; Division can throw the error DIVISION-BY-ZERO.
  (/ (- (cdr p) (cdr q))
     (- (car p) (car q))))

;; (defun compute-slopes (points)
;;   "Compute the slope between successive points in POINTS."
;;   (loop :for p :in points
;;         :for q :in (cdr points)
;;         :collect (handler-case (slope p q)
;;                    (division-by-zero () nil))))

(defun compute-slopes (points)
  "Compute the slope between successive points in POINTS."
  (loop :for p :in points
        :for q :in (cdr points)
        :collect (restart-case (slope p q)
                   (return-nil ()
                     :report "Return NIL."
                     nil)
                   (return-zero ()
                     :report "Return zero."
                     0)
                   (specify-value (value)
                     :report "Specify a value to return."
                     :interactive (lambda ()
                                    (format t "Enter a value: ")
                                    (list (read)))
                     value))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; USER CODE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *point-data* '(( 1 . 1) ( 3 . -2) (6 . 4) (6 . -1) (8 . 9)
                       (-3 . 0) (-2 .  0) (2 . 2) (0 .  6) (0 . 0)))

(defun slope-to-angle (slope)
  "Convert a slope value SLOPE into an angle in degrees."
  (values (round (* 180 (atan slope))
                 pi)))

(defun compute-angles (points)
  "Compute the angles of the lines (to the horizontal) that connect
  adjacent points in POINTS."
  (mapcar #'slope-to-angle (compute-slopes points)))

(defun work ()
  "Do all of the work."
  (handler-bind
      ;; Catch any division errors.
      ((division-by-zero (lambda (c)
                           (declare (ignore c))
                           ;; Provide a very high slope.
                           (invoke-restart 'specify-value 150))))
    ;; Perform the work.
    (compute-angles *point-data*)))

