;;;; stack-queue.lisp
;;;; Copyright (c) 2012 Robert Smith

;;;; Implementation of stacks and queues.

;;; Stacks

(defstruct (stack (:constructor %make-stack)
                  (:predicate stackp))
  elements)

(defun make-stack ()
  "Create a new empty stack."
  (%make-stack))

(defun stack-push (stack obj)
  "Push the element OBJ onto the stack STACK."
  (push obj (stack-elements stack))
  stack)

(defun stack-pop (stack)
  "Pop an element off the stack STACK."
  (pop (stack-elements stack)))


;;; Queues

(defstruct (queue (:constructor %make-queue)
                  (:predicate queuep))
  elements
  last)

(defun make-queue ()
  "Create a new empty queue."
  (%make-queue))

(defun queue-empty-p (queue)
  "Is the queue QUEUE empty?"
  (null (queue-elements queue)))

(defun list-to-queue (list)
  "Convert the list LIST into a queue. Note: LIST may be modified."
  (make-queue :elements list
              :last (last list)))

(defun enqueue (queue obj)
  "Add an element OBJ to the end of the queue QUEUE."
  (let ((last (list obj)))
    (if (queue-empty-p queue)
        (setf (queue-last queue)     last
              (queue-elements queue) last)
        (setf (cdr (queue-last queue)) last
              (queue-last queue)       last)))
  queue)

(defun dequeue (queue)
  "Remove and return an element from the queue QUEUE."
  (pop (queue-elements queue)))


