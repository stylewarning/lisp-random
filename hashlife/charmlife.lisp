;;;; charmlife.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

(defpackage #:charmlife
  (:use #:cl #:hashlife)
  (:export #:main))

(in-package #:charmlife)

(defconstant +universe-level+ 60)

(defvar *universe* (empty-macrocell +universe-level+))

(defconstant +hyperstep-size+ (expt 2 (- +universe-level+ 2)))

(defvar *generation* 0)

;;; universe-x = screen-x + delta-x

(defun paint-universe ()
  "Paint the universe."
  (charms:clear-window charms:*standard-window*)
  (multiple-value-bind (width height)
      (charms:window-dimensions charms:*standard-window*)
    (let ()
      ;; Paint the universe
      (loop :for screen-y :below height :do
        (loop :for screen-x :below width :do
          
          (when (= 1 (macrocell-cell *universe*
                                     (- screen-x (floor width 2))
                                     (- screen-y (floor height 2))))
            (charms:write-char-at-point charms:*standard-window* #\* screen-x screen-y))))
      ;; Paint generation number.
      (charms:write-string-at-point charms:*standard-window*
                                    (format nil "Generation: ~D" *generation*)
                                    0
                                    0))))

(defun toggle-cell (universe-x universe-y)
  (setf *universe* (macrocell-set-cell (- 1 (macrocell-cell *universe* universe-x universe-y))
                                       *universe*
                                       universe-x
                                       universe-y)))

(defun main ()
  "Start the Game of Life."
  (charms:with-curses ()
    (charms:disable-echoing)
    (charms:enable-raw-input :interpret-control-characters t)
    (charms:enable-non-blocking-mode charms:*standard-window*)

    (paint-universe)
    (loop :named driver-loop
          :with universe-center-x := 0
          :with universe-center-y := 0
          :with x := 0                  ; Cursor X coordinate
          :with y := 0                  ; Cursor Y coordinate
          :for c := (charms:get-char charms:*standard-window*
                                     :ignore-error t)
          :do (progn
                ;; Refresh the window
                (charms:refresh-window charms:*standard-window*)

                ;; Process input
                (case c
                  ((nil) nil)
                  ((#\w) (decf y))
                  ((#\a) (decf x))
                  ((#\s) (incf y))
                  ((#\d) (incf x))
                  ((#\Space)
                   (multiple-value-bind (width height)
                       (charms:window-dimensions charms:*standard-window*)
                     (toggle-cell (- x (floor width 2))
                                  (- y (floor height 2))))
                   (paint-universe))
                  #+#:broken
                  ((#\1)
                   (setf *universe* (pad-macrocell (timestep *universe*)))
                   (incf *generation* 1)
                   (paint-universe))
                  ((#\!)
                   (setf *universe* (pad-macrocell (hyperstep *universe*)))
                   (incf *generation* +hyperstep-size+)
                   (paint-universe))
                  ((#\q #\Q) (return-from driver-loop)))

                ;; Normalize the cursor coordinates
                (multiple-value-bind (width height)
                    (charms:window-dimensions charms:*standard-window*)
                  (setf x (mod x width)
                        y (mod y height)))

                ;; Move the cursor to the new location
                (charms:move-cursor charms:*standard-window* x y)))))

