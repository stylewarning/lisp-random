;;;; real-time-counter.lisp
;;;; Copyright (c) 2013 Robert Smith

(defconstant resolution (expt 10 (floor (log internal-time-units-per-second 10))))

(defun now ()
  (values (round (* resolution (get-internal-real-time))
                 internal-time-units-per-second)))

(defun make-adjustable-vector ()
  (make-array 5 :element-type 'unsigned-byte
                :initial-element 0
                :adjustable t
                :fill-pointer 0))

(defstruct real-time-counter
  (ticks 0 :type unsigned-byte)
  (timestamps (make-adjustable-vector) :type (and (vector unsigned-byte)
                                                  (not simple-array))))

(defun increment (rtc)
  (vector-push-extend (now) (real-time-counter-timestamps rtc))
  (incf (real-time-counter-ticks rtc)))

(defun get-ticks-in-last (rtc interval)
  "Get the number of ticks of the counter RTC in the last INTERVAL seconds."
  (let* ((past       (- (now) (round (* resolution interval))))
         (timestamps (real-time-counter-timestamps rtc))
         (ticks      (real-time-counter-ticks rtc)))
    ;; Find the lowest time whose value is greater than or equal to PAST.
    (labels ((bisect (low high)
               ;; (format t "BISECT: ~A -- ~A~%" low high)
               (let ((low-time (aref timestamps low))
                     (high-time (aref timestamps high)))
                 (cond
                   ((> past high-time) nil)
                   ((= low high) low)
                   ((= 1 (- high low)) (if (< low-time past)
                                           high
                                           low))
                   (t (let* ((mid (floor (+ high low) 2))
                             (mid-time (aref timestamps mid)))
                        (if (< mid-time past)
                            (bisect mid high)
                            (bisect low mid))))))))
      (if (zerop ticks)
          0
          (let ((b (bisect 0 (1- ticks))))
            (if b
                (- ticks (bisect 0 (1- ticks)))
                0))))))

(defun get-test-rtc ()
  (let ((rtc (make-real-time-counter)))
    (increment rtc)
    (sleep 2)
    (dotimes (i 5)
      (increment rtc))
    rtc))
