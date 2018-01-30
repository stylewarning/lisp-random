;;;; rle.lisp
;;;;
;;;; Copyright (c) 2017^W2018 Robert Smith

;;; In this file, we explore a few solutions to the problem of
;;; calculating the "run-length encoding" of a list of numbers. A
;;; description of the exercise can be found here:
;;;
;;;    http://www.watrophy.com/posts/24-Run-Length-Encoding.html

;;; SOLUTION #1: The Honest and Correct Solution (TM)
;;;
;;; In this first solution, our algorithm for computing the RLE has a
;;; couple of nice properties.
;;;
;;;     1. It is explicitly and obviously type safe.
;;;
;;;        The functions RLE and %RLE have the following types, using
;;;        a mix of Haskell/Standard ML notation:
;;;
;;;           RLE :: [Number] -> [(Number, Integer)]
;;;
;;;           %RLE :: [Number] * Number * Integer * [(Number, Integer)] -> [(Number, Integer)]
;;;
;;;        (Here, the products are actually function arguments,
;;;        whereas the tuples are cons cells.)
;;;
;;;        Every operation within each function properly distnguish
;;;        and handle the types (e.g., elements of an empty list are
;;;        never attempted to be accessed).
;;;
;;;     2. The COLLECTED argument always contains correct data that
;;;        coincides with the output, albeit in reversed order. At no
;;;        time is this invariant ever broken. Even stronger, however,
;;;        is that the COLLECTED argument will contain the maximum
;;;        amount of guaranteed correct data throughout the course of
;;;        the algorithm.
;;;
;;;     3. The functions are tail-recursive.
;;;
;;; This leads to a solution which is safe and easy to understand, but
;;; a bit repetitive and verbose.

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


;;; SOLUTION #2: Using REDUCE
;;;
;;; The keen reader will notice that the type signature and
;;; implementation of %RLE is reminiscent to a fold or reduce. In
;;; particular, we are traversing a list (LIST), whose elements are
;;; being looked at (NEXT-ITEM), while something is being computed
;;; with those elements (ITEM, COUNT, and COLLECTED). Since REDUCE
;;; requires a binary function, we need to wrap what we are computing
;;; up into a structure.
;;;
;;; We choose to dreate a named throwaway structure (hence the :TYPE
;;; LIST) which has the state being collected in the reduction. We
;;; could very well just use lists. (In general, I prefer to use
;;; function arguments, as in Solution #1, in a language like Lisp
;;; instead of temporary structures.)
(defstruct (rle-state (:type list)
                      (:constructor rle-state (item count collected)))
  item
  count
  collected)

;;; Now we write the reducing function. The type signature of REDUCE
;;; is
;;;
;;;     REDUCE :: (S * A -> S) * [A] -> S.
;;;
;;; In our case, S will be RLE-STATE and A will be NUMBER.
(defun ponder-next-item (state next-item)
  (if (= next-item (rle-state-item state))
      (rle-state (rle-state-item state)
                 (1+ (rle-state-count state))
                 (rle-state-collected state))
      ;; Note that we are collecting in reverse order.
      (rle-state next-item
                 1
                 (acons (rle-state-item state)
                        (rle-state-count state)
                        (rle-state-collected state)))))

(defun rle-from-state (state)
  (reverse
   (acons (rle-state-item state)
          (rle-state-count state)
          (rle-state-collected state))))

;;; Now we can implement RLE. Note that we check if the list is empty
;;; because otherwise we cannot write the REDUCE call with the correct
;;; type.
(defun rle2 (list)
  (if (null list)
      nil
      (rle-from-state
       (reduce #'ponder-next-item (rest list)
               :initial-value (rle-state (first list) 1 nil)))))

;;; While it feels nice to use high-order functions, we've done
;;; ourselves a disservice by introducing intermediate state, and
;;; performing all of this packing and unpacking.


;;; SOLUTION #3: Whole-Hog Lisp
;;;
;;; For when you feel like it's 1958.
(defun rle3 (list)
  (loop :while list
        :for tail := (member (first list) list :test-not #'=)
        :for run := (ldiff list tail)
        :collect (cons (first run) (length run))
        :do (setf list tail)))


;;; SOLUTION #4: Mapping the RLE
;;;
;;; In this solution, we *map* across the elements of the RLE. We use
;;; Solution #1 as a basis.
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

(defun rle4 (list)
  (let ((collected nil))
    (map-rle (lambda (item count) (push (cons item count) collected)) list)
    (nreverse collected)))


;;; SOLUTION #5: Using lazy sequences
;;;
;;; This is a straightforward translation of soluton #1 using lazy
;;; primitives.

(load "delay.lisp")

(defun rle5 (list)
  (lazy:force-tree (rle-lazy nil list)))

(defun rle-lazy (limit list)
  (check-type limit (or null (integer 1)))
  (if (null list)                       ; Ensure we have one element.
      nil
      (lazy:delay (%rle-lazy limit
                             (lazy:cdr list)
                             (lazy:car list)
                             1))))

(defun %rle-lazy (limit list item count)
  (if (null list)
      (acons item count nil)
      (let ((next-item (lazy:car list))
            (rest-list (lazy:cdr list)))
        (if (and (= item next-item)
                 (or (null limit)
                     (< count limit)))
            (%rle-lazy limit rest-list next-item (1+ count))
            (lazy:delay (acons item count (%rle-lazy limit rest-list next-item 1)))))))


;;; Tests for finite-length RLE

(defparameter *rle-tests*
  '(
    nil              nil
    (1)              ((1 . 1))
    (1 2)            ((1 . 1) (2 . 1))
    (1 1)            ((1 . 2))
    (1 1 2)          ((1 . 2) (2 . 1))
    (1 2 2)          ((1 . 1) (2 . 2))
    (1 1 1 2 3 3 3)  ((1 . 3) (2 . 1) (3 . 3))))

(defun |122333| (&optional (start 1))
  [lazy:append (lazy:take start (lazy:repeat start))
               (|122333| (1+ start))])

(defun test-rle ()
  (loop :for (in out) :on *rle-tests* :by #'cddr
        :do (assert (equal out (rle in)))
            (assert (equal out (rle2 in)))
            (assert (equal out (rle3 in)))
            (assert (equal out (rle4 in)))
            (assert (equal out (rle5 in))))

  ;; will not terminate, as expected:
  #+ignore (lazy:take 1 (rle-lazy nil (lazy::repeat 1)))
  (assert (equal '((1 . 1) (2 . 2) (3 . 3))
                 (lazy:force-tree
                  (lazy:take 3 (rle-lazy nil (|122333|))))))
  (assert (equal '((1 . 1) (2 . 2) (3 . 2) (3 . 1) (4 . 2))
                 (lazy:force-tree
                  (lazy:take 5 (rle-lazy 2 (|122333|))))))
  (assert (equal (lazy:force-tree (lazy:take 10 (rle-lazy 2 (lazy:repeat 1))))
                 (lazy:force-tree (lazy:take 10 (lazy:repeat (cons 1 2))))))
  'success)
