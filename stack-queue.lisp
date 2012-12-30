;;;; stack-queue.lisp
;;;; Copyright (c) 2012 Robert Smith

;;;; Implementation of stacks and queues.

;;; Stacks

(defstruct (stack (:constructor %make-stack)
                  (:predicate stackp))
  (elements nil :type list))

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
  (elements nil :type list)
  (last nil :type (or null (cons t null))))

(defun make-queue ()
  "Create a new empty queue."
  (%make-queue))

(defun queue-empty-p (queue)
  "Is the queue QUEUE empty?"
  (null (queue-elements queue)))

(defun list-to-queue (list)
  "Convert the list LIST into a queue. Note: LIST may be modified."
  (%make-queue :elements list
               :last (last list)))

(defun enqueue (queue obj)
  "Add an element OBJ to the end of the queue QUEUE."
  (let ((last (list obj)))
    (if (queue-empty-p queue)
        ;; Set up the queue with the first element. Note that the same
        ;; reference to the singleton list is shared by both
        ;; QUEUE-ELEMENTS and QUEUE-LAST.
        (setf (queue-elements queue) last
              (queue-last queue)     last)
        
        ;; We can now append elements to QUEUE-ELEMENTS simply by
        ;; modifying QUEUE-LAST, whose reference is shared by
        ;; QUEUE-ELEMENTS,
        ;;
        ;; We do this instead of a single SETF for type safety of
        ;; QUEUE-LAST.
        (let ((old (queue-last queue)))
          (setf (queue-last queue) last
                (cdr old)          last))))
  queue)

(defun dequeue (queue)
  "Remove and return an element from the queue QUEUE."
  (pop (queue-elements queue)))

