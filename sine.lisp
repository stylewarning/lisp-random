;;;; Accurate curve drawing derived from from Toby Thain's 'sine.ps'
;;;; located at
;;;;       <http://www.telegraphics.com.au/sw/info/sine.html>

;;;; Copyright (C) 2011 Robert Smith
;;;;
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software
;;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

(defpackage #:diffcurve
  (:use #:cl #:vecto)
  (:export :draw-curve))

(in-package #:diffcurve)

;;; TODO:
;;; Needs major cleaning up. Need to be able to pass
;;;    XMIN XMAX YMIN YMAX : the window
;;;    DX                  : the step
;;;    F DF                : function and derivative
;;; as arguments with correct zooming.
(defun draw-curve (file &optional (w 500) (h 500))
  "Draw a curve as a png to FILE with width W and height H (in
pixels). LIMITATION: Only draws SIN at the moment."
  (let* ((f #'(lambda (x) (* 50 (sin (/ x 50)))))
         (df #'(lambda (x) (cos (/ x 50))))
         (dx (/ pi 4 1/100))
         (third (/ dx 3)))
    (vecto:with-canvas (:width w :height h)
      (vecto:translate 0 (/ h 2))
      (vecto:move-to 0 (funcall f 0))
      (dotimes (i (ceiling w dx))
        (let* ((x (* dx i))
               (x+dx (+ x dx)))
          (vecto:curve-to
           (+ x third)
           (+ (funcall f x) (* third (funcall df x)))
           (- x+dx third)
           (- (funcall f x+dx) (* third (funcall df x+dx)))
           x+dx
           (funcall f x+dx))
          (vecto:stroke)
          (vecto:move-to x+dx (funcall f x+dx))))
      (vecto:save-png file))))

