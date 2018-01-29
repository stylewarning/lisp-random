;;;; rle.lisp
;;;;
;;;; Copyright (c) 2017 Robert Smith

;;; Compute the run length encoding of a list.

(defun rle (list)
  (if (null list)                       ; Ensure we have one element.
      nil
      (%rle (rest list) (first list) 1 nil)))

(defun %rle (list item count collected)
  (if (null list)
      ;; Finished the list. Wrap up and ship off.
      (nreverse (acons item count collected))
      ;; Continue with business. Check the next element of the list
      ;; and recurse.
      (destructuring-bind (next-item . rest-list) list
        (if (= item next-item)
            ;; The item is the same. Bump the count.
            (%rle rest-list next-item (1+ count) collected)
            ;; The item is different. Collect it.
            (%rle rest-list next-item 1 (acons item count collected))))))

;;; Variant for potentially infinite streams.
;;;
;;; Here F is a binary function which takes the item and the number of
;;; times it was found in the run.
(defun map-rle (f list)
  (if (null list)                       ; Ensure we have one element.
      nil
      (%map-rle f (rest list) (first list) 1)))

(defun %map-rle (f list item count)
  (if (null list)
      (funcall f item count)
      (destructuring-bind (next-item . rest-list) list
        (if (= item next-item)
            (%map-rle f rest-list next-item (1+ count))
            (progn
              (funcall f item count)
              (%map-rle f rest-list next-item 1))))))

;;; Test
(defparameter *rle-tests*
  '(
    nil              nil
    (1)              ((1 . 1))
    (1 2)            ((1 . 1) (2 . 1))
    (1 1)            ((1 . 2))
    (1 1 2)          ((1 . 2) (2 . 1))
    (1 2 2)          ((1 . 1) (2 . 2))
    (1 1 1 2 3 3 3)  ((1 . 3) (2 . 1) (3 . 3))))

(defun test-rle ()
  (loop :for (in out) :on *rle-tests* :by #'cddr
        :do (assert (equal out (rle in))))
  'success)
