;;;; binary-tree-traversal.lisp
;;;; Copyright (c) 2012 Robert Smith

;;;; This file has the solution to basic computer science exercises on
;;;; tree traversal. It includes the tree representation,
;;;; construction, pre/in/post-order (depth-first) traversal, and
;;;; level-order (breadth-first) traversal.

(defstruct node
  value
  left
  right)

(defun build-tree (list)
  (cond
    ((null list) nil)
    ((atom list) (make-node :value list))
    ((listp list) (destructuring-bind (v l r) list
                    (make-node :value v
                               :left (build-tree l)
                               :right (build-tree r))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Depth First ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pre-order (f tree)
  (let ((value (node-value tree))
        (left  (node-left tree))
        (right (node-right tree)))
    ;; 1. Root
    (funcall f value)
    
    ;; 2. Left
    (when left
      (pre-order f left))
    
    ;; 3. Right
    (when right
      (pre-order f right)))

  ;; Return NIL.
  nil)

(defun in-order (f tree)
  (let ((value (node-value tree))
        (left  (node-left tree))
        (right (node-right tree)))

    ;; 1. Left
    (when left
      (in-order f left))

    ;; 2. Root
    (funcall f value)
        
    ;; 3. Right
    (when right
      (in-order f right)))

  ;; Return NIL.
  nil)

(defun post-order (f tree)
  (let ((value (node-value tree))
        (left  (node-left tree))
        (right (node-right tree)))

    ;; 1. Left
    (when left
      (post-order f left))
    
    ;; 2. Right
    (when right
      (post-order f right))

    ;; 3. Root
    (funcall f value))

  ;; Return NIL.
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;; Breadth First ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; N.B. This is not an efficient queue implementation. DEQUEUE takes
;;; O(N) time. Implementing a heap would be a better idea.

(defstruct queue
  elements)

(defun enqueue (queue item)
  (setf (queue-elements queue)
        (push item (queue-elements queue))))

(defun dequeue (queue)
  (let ((elts (queue-elements queue)))
    (prog1 (if (null elts)
               nil
               (first (last elts)))
      (setf (queue-elements queue)
            (butlast elts)))))

(defun level-order (f tree)
  (loop :with queue := (make-queue :elements (list tree))
        :until (null (queue-elements queue))
        :do (let* ((next (dequeue queue))
                   (left (node-left next))
                   (right (node-right next)))
              
              ;; Perform operation on current node.
              (funcall f (node-value next))
              
              ;; Queue up new nodes at the next level.
              (when left
                (enqueue queue left))
              
              (when right
                (enqueue queue right)))
        :finally (return nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *test-tree*
  (build-tree `(f (b (a nil nil)
                     (d (c nil nil)
                        (e nil nil)))
                  (g nil
                     (i (h nil nil)
                        nil)))))

(defparameter *test-tree-2*
  (build-tree `(f (b a 
                     (d c e))
                  (g nil
                     (i h nil)))))

;;; CL-USER> (pre-order #'princ *test-tree*)
;;; FBADCEGIH
;;; NIL
;;; CL-USER> (in-order #'princ *test-tree*)
;;; ABCDEFGHI
;;; NIL
;;; CL-USER> (post-order #'princ *test-tree*)
;;; ACEDBHIGF
;;; F
;;; CL-USER> (level-order #'princ *test-tree*)
;;; FBGADICEH
;;; NIL