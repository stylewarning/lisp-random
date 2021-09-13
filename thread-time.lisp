;;;; thread-time.lisp
;;;;
;;;; Copyright (c) 2021 Robert Smith

;;; This file computes how long a thread has been active.
;;;
;;; See the documentation for THREAD-TIME:THREAD-CPU-TIME.

;;;  Requires UIOP and SPLIT-SEQUENCE to be loaded.

#-(and sbcl linux) (error "this only works on linux and SBCL")

(defpackage #:thread-time
  (:use #:cl)
  (:export #:thread-cpu-time))

(in-package #:thread-time)

(sb-ext:defglobal **ticks-per-second**
    (parse-integer (uiop:run-program "getconf CLK_TCK" :output '(:string :stripped t))))

(defun ticks->millis (x)
  (round (* 1000 x) **ticks-per-second**))

(defun thread-tid (thread)
  (sb-thread:thread-os-tid thread))

(defun thread-cpu-time (&optional (thread sb-thread:*current-thread*))
  "Calculate statistics for the amount of time THREAD (default: the current thread) has been running. Return 3 values:

1. The number of milliseconds the thread has been active total.

2. The number of milliseconds the thread has been active in user-mode.

3. The number of milliseconds the thread has been active in kernel-mode.

Note that the times returned by this function may have less than a millisecond resolution."
  ;; see `man proc` for details
  (let ((tid (thread-tid thread)))
    (if (null tid)
        nil
        (let ((path (format nil "/proc/self/task/~D/stat" (thread-tid thread))))
          (with-open-file (s path :direction ':input :if-does-not-exist ':error)
            (let* ((line (read-line s))
                   (proc-entry
                     (nthcdr (- 14 2)
                             (split-sequence:split-sequence
                              #\Space
                              line
                              :start (1+ (position #\) line))))))
              ;; The second item in proc is the name of the thread
              ;; which looks like
              ;;
              ;;     23456 (name of thread) S ...
              ;;
              ;; So we lop off everything until then to make string
              ;; splitting easier, then lop off the first 13 entries.
              ;;
              ;; 14th entry = user-mode CPU time
              ;;
              ;; 15th entry = kernel-mode CPU time
              (let ((user-time (parse-integer (first proc-entry)))
                    (kern-time (parse-integer (second proc-entry))))
                (values (ticks->millis (+ user-time kern-time))
                        (ticks->millis user-time)
                        (ticks->millis kern-time)))))))))
