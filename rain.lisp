;;;; rain.lisp
;;;;
;;;; Copyright (c) 2018 Robert Smith

;;; Idea is that the water above a building is going to be
;;;
;;;     (- (MIN TALLEST-LEFT TALLEST-RIGHT) HEIGHT)
;;;
;;; where TALLEST-LEFT is the tallest building to the left of the
;;; current building, TALLEST-RIGHT is the tallest building to the
;;; right of the current building, and HEIGHT is the height of the
;;; current building.
;;;
;;; Calculating this formula naively for each element will be O(n^2)
;;; space. We can reduce that by just remembering the heights in a
;;; secondary set of arrays, but that will still be O(n) space.
;;;
;;; So far, we've assumed that we'd walk the array in one
;;; direction. What if, instead, we walked the array from both the
;;; left and the right, closing in by one each time? In this way, we'd
;;; have two pointers, LEFT and RIGHT. While tracing through with
;;; these pointers, we remember TALLEST-LEFT and TALLEST-RIGHT that
;;; we've encountered.
;;;
;;; TALLEST-LEFT will *only* be valid for the LEFT element, and
;;; TALLEST-RIGHT will *only* be valid for the RIGHT element. So how
;;; do we deal with the MIN computation?
;;;
;;; If the element at LEFT is smaller than the element at RIGHT, then
;;; we can only add water based off of TALLEST-LEFT. We don't need to
;;; consider TALLEST-RIGHT because we've established we are at least
;;; as tall as the element at RIGHT, which gives us a guarantee that
;;;
;;;     TALLEST-RIGHT >= RIGHT
;;;
;;; which is enough. The same argument applies when the inequalities
;;; are reversed, but the argument applies to the opposite sides.
;;;
;;; We terminate as soon as the pointers have walked past one another.

(defun make-it-rain (buildings)
  (labels ((height (i) (aref buildings i))

           (narrow (rainwater left tallest-left right tallest-right)
             (cond
               ;; We've seen every building. Return.
               ((> left right) rainwater)

               ;; Process the left element.
               ((< (height left) (height right))
                (if (< tallest-left (height left))
                    (narrow rainwater
                            (1+ left) (height left) right tallest-right)
                    (narrow (+ rainwater (- tallest-left (height left)))
                            (1+ left) tallest-left right tallest-right)))

               ;; Process the right element.
               (t
                (if (< tallest-right (height right))
                    (narrow rainwater
                            left tallest-left (1- right) (height right))
                    (narrow (+ rainwater (- tallest-right (height right)))
                            left tallest-left (1- right) tallest-right))))))
    (narrow 0 0 0 (1- (length buildings)) 0)))

(defun test ()
  (assert (= 0 (make-it-rain #())))
  (assert (= 0 (make-it-rain #(1))))
  (assert (= 0 (make-it-rain #(1 1))))
  (assert (= 0 (make-it-rain #(1 1 1))))
  (assert (= 1 (make-it-rain #(1 0 1))))
  (assert (= 1 (make-it-rain #(2 0 1))))
  (assert (= 1 (make-it-rain #(3 0 1))))
  (assert (= 1 (make-it-rain #(3 3 0 1))))
  (assert (= 2 (make-it-rain #(1 0 1 0 1))))
  (assert (= 2 (make-it-rain #(1 0 2 0 1))))
  (assert (= 8 (make-it-rain #(3 0 1 0 3)))))
