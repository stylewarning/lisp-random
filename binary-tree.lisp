;;;; binary-tree.lisp
;;;; Copyright (c) 2013 Robert Smith

;;;; Load what we need.

(quote #.(ql:quickload :cl-algebraic-data-type))


;;;; Binary tree implementation.

(adt:defdata binary-tree
  (node integer binary-tree binary-tree)
  leaf)                                 ; a LEAF is just a terminal node

(defun binary-insert (tr val)
  "Insert the integer VAL into the tree TR."
  (adt:match binary-tree tr
    ((node n left right) (if (<= val n)
                             (node n (binary-insert left val) right)
                             (node n left (binary-insert right val))))
    (leaf (node val leaf leaf))))

(defun build-tree (vals)
  "Build a tree from the list of integer values VALS."
  (reduce #'binary-insert vals :initial-value leaf))

(defun binary-search (tr val)
  "Check if the integer VAL is a member of the tree TR."
  (adt:match binary-tree tr
    ((node n left right) (cond
                           ((= val n) t)
                           ((< val n) (binary-search left val))
                           (t         (binary-search right val))))
    (leaf nil)))

;;; EXAMPLE
;;; -------
;;;
;;; > (build-tree '(1 2 3 4))
;;; #<NODE 1 #<LEAF> #<NODE 2 #<LEAF> #<NODE 3 #<LEAF> #<NODE 4 #<LEAF> #<LEAF>>>>>
;;; > (binary-search * 3)
;;; T
;;; > (binary-search ** 8)
;;; NIL
