;;;; falling-leaves.lisp
;;;;
;;;; Copyright (c) 2018 Robert Smith

;;; The problem is described in
;;;
;;; http://www.watrophy.com/posts/36-Falling-Leaves.html
;;;
;;; Short story is, if for a tree T, FALL(T) is T with all leaves
;;; removed, then compute the sequence l_i(T) = (leaves ∘ fallⁱ) (T)
;;; for 0 ≤ i ≤ n, where fallⁿ(T) is the root.

(defun name (node) (first node))
(defun children (node) (rest node))
(defun leafp (node) (null (children node)))

;; The main thing is distance to leaves. For any node n, the distance
;; to the leaves can be calculated by
;;
;; dist(n) = 1 + max{dist(c) : c ∈ children(n) }.
;;
;; We don't use this function, but it demonstrates this recursion.
(defun leaf-distance (node)
  (if (leafp node)
      0
      (reduce #'max (children node) :key #'leaf-distance)))

(defun record-leaf-distance (node)
  (let ((record (make-array 1 :initial-element nil :adjustable t)))
    (labels ((record (name dist)
               (when (<= (length record) dist)
                 (adjust-array record (1+ dist) :initial-element nil))
               (push name (aref record dist))
               dist)
             (dist (node)
               (record
                (name node)
                (if (leafp node)
                    0
                    (1+ (reduce #'max (children node) :key #'dist))))))
      (dist node)
      ;; This MAP isn't necessary, but it makes the ordering as if
      ;; read left-to-right.
      (map 'list #'reverse record))))

;;; Example

(defvar *example* '(a
                    (b
                     (d
                      (h)
                      (i))
                     (e))
                    (c
                     (f
                      (j))
                     (g))))

;; CL-USER> (record-leaf-distance *example*)
;; ((H I E J G) (D F) (B C) (A))
