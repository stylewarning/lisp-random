;;;; circular-queue.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

;;;; Implementation of the queue data structure using a circularly
;;;; singly linked list.
;;;;
;;;; This only uses a single pointer to keep track of the queue as
;;;; opposed to two, as in "stack-queue.lisp".

(defstruct (queue (:print-function
                   (lambda (obj stream depth)
                     (declare (ignore depth))
                     (print-unreadable-object (obj stream :type t :identity t))))
                  (:constructor make-queue nil)
                  (:predicate queuep)
                  (:copier nil))
  (tail nil :type (or null cons)))

(defun queue-empty-p (q)
  "Is the queue Q empty?"
  (null (queue-tail q)))

(defun queue-push (obj q)
  "Push an object OBJ onto the tail of the queue Q."
  (if (queue-empty-p q)
      ;; Construct a new tail which is also the head.
      (let ((tail (cons obj nil)))
        (setf (queue-tail q) tail
              (cdr tail)     tail))
      ;; Construct a new tail, which points to the current
      ;; head. Modify the previous tail to point to the new tail.
      (let* ((tail (queue-tail q))
             (head (cdr tail))
             (new-tail (cons obj head)))
        (setf (cdr tail) new-tail
              (queue-tail q) new-tail))))

(defun queue-pop (q)
  "Pop an object from the head of the queue Q. Return the object. If the queue is empty, NIL is returned."
  (if (queue-empty-p q)
      nil
      (let* ((tail (queue-tail q))
             (head (cdr tail))
             (obj (car head)))
        ;; Skip the head if >1 element or null out the queue
        ;; otherwise.
        (if (eq head tail)
            (setf (queue-tail q) nil)
            (setf (cdr tail) (cdr head)))
        ;; Return the object.
        obj)))

(defun map-queue (f q)
  "Apply the unary function F to each element of the queue Q in order. Return the queue."
  (unless (queue-empty-p q)
    (loop :with tail := (queue-tail q)
          :for node := (cdr tail) :then (cdr node)
          :do (funcall f (car node))
          :until (eq node tail)))
  ;; Return the queue.
  q)

(defun queue-length (q)
  "Compute the number of elements in the queue."
  (let ((length 0))
    (map-queue (lambda (el)
                 (declare (ignore el))
                 (incf length))
               q)
    length)
  #+#:non-functional-way
  (if (queue-empty-p q)
      0
      (loop :with tail := (queue-tail q)
            :for len :from 1
            :for node := (cdr tail) :then (cdr node)
            :until (eq node tail)
            :finally (return len))))

(defun copy-queue (q)
  "Make a shallow copy of the queue Q."
  ;; It seems most expedient, if a bit less efficient, to just make a
  ;; new queue and push all of the elements from the old on one.
  (let ((copy (make-queue)))
    (map-queue (lambda (el)
                 (queue-push el copy))
               q)
    copy))
