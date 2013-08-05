;;;; pythagorean-triples.lisp
;;;; Copyright (c) 2013 Robert Smith

;;;; A program to generate Pythagorean triples.

;;;;;;;;;;;;;;;;;;;;;;; FROM stack-queue.lisp ;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;; END stack-queue.lisp ;;;;;;;;;;;;;;;;;;;;;;;;

;;; [ q  q' ]   [   q         q'   ]
;;; [       ] = [                  ]
;;; [ p  p' ]   [ q + q'   2q + q' ]

(defun box (q q-prime p p-prime)
  (vector q q-prime p p-prime))

;;; Type-1 Box
;;;
;;; [ * x ]
;;; [ * y ]

(defun solve-type-1 (box)
  (let* ((x (aref box 1))
         (y (aref box 3))
         (q (floor (- y x) 2)))
    (setf (aref box 0) q)
    (setf (aref box 2) (+ q x))
    box))

;;; Type-2 Box
;;;
;;; [ * x ]
;;; [ y * ]

(defun solve-type-2 (box)
  (let* ((x (aref box 1))
         (y (aref box 2))
         (q (- y x)))
    (setf (aref box 0) q)
    (setf (aref box 3) (+ q y))
    box))

;;; Type-3 Box
;;;
;;; [ x y ]
;;; [ * * ]

(defun solve-type-3 (box)
  (let* ((x (aref box 0))
         (y (aref box 1))
         (p (+ x y)))
    (setf (aref box 2) p)
    (setf (aref box 3) (+ p x))
    box))


(defun box-to-pythag (box)
  (let ((q       (aref box 0))
        (q-prime (aref box 1))
        (p       (aref box 2))
        (p-prime (aref box 3)))
    (let ((a (* 2 p q))
          (b (* q-prime p-prime))
          (c (- (* p p-prime)
                (* q q-prime))))
      (sort (list a b c) #'<))))


;;; BOX must me a box whose second column has coprime, odd integers.
(defun triples-for (n box)
  (let ((queue (make-queue)))
    (flet ((enqueue-next-boxes (box)
             (let ((x (aref box 1))
                   (y (aref box 3)))
               (enqueue queue (solve-type-2 (box nil x y nil)))
               (enqueue queue (solve-type-3 (box x y nil nil)))
               (enqueue queue (solve-type-3 (box y x nil nil))))))
      (loop :repeat n
            :for next-box := box :then (dequeue queue)
            :do (enqueue-next-boxes next-box)
            :collect (box-to-pythag next-box)))))

;;; Test

(defun ensure-pythagoran-triples (triples)
  (every (lambda (triple)
           (destructuring-bind (a b c) triple
             (format t "~A^2 + ~A^2 = ~A =? ~A~%" a b (+ (* a a) (* b b)) (* c c))
             (= (* c c) (+ (* a a) (* b b)))))
         triples))
