;;;; completion.lisp
;;;; Copyright (c) 2011 Robert Smith

;;;; String completions and stuff

(ql:quickload #:qtility)

;;;;;;;;;;;;;;;;;;;;;; Completion node datatype ;;;;;;;;;;;;;;;;;;;;;;

(defstruct (completion-node (:conc-name completion-node.)
                            (:print-function completion-node-printer))
  (char   #\nul :type base-char :read-only t)
  (endp   nil   :type boolean)
  (left   nil   :type (or null completion-node))
  (middle nil   :type (or null completion-node))
  (right  nil   :type (or null completion-node)))

(defun completion-node-printer (obj stream depth)
  "Printer for ternary nodes."
  (declare (ignore depth))
  (print-unreadable-object (obj stream :type t :identity t)
    (princ "CHAR=" stream)
    (print (completion-node.char obj) stream)
    (princ " ENDP=" stream)
    (princ (completion-node.endp obj) stream)))

(defun completion-node (char)
  "Make a fresh ternary node. with character CHAR."
  (make-completion-node :char char))


;;;;;;;;;;;;;;;;;;;;;; Completion tree datatype ;;;;;;;;;;;;;;;;;;;;;;

(defstruct (completion-tree (:conc-name completion-tree.)
                            #+#:ignore
                            (:print-function completion-tree-printer))
  (root nil :type (or null completion-node)))

(defun completion-tree-printer (obj stream depth)
  "Printer for ternary trees."
  (declare (ignore depth))
  (print-unreadable-object (obj stream :type t :identity t)))


;;;;;;;;;;;;;;;;;;;; Completion tree modification ;;;;;;;;;;;;;;;;;;;;

(defun completion-tree-add (tree str)
  "Add a string STR to the ternary tree TREE."
  (labels ((completion-tree-add-node (pos node)
             (cond 
               ((char< (aref str pos)
                       (completion-node.char node))
                (unless (completion-node.left node)
                  (setf (completion-node.left node)
                        (completion-node (aref str pos))))
                (completion-tree-add-node pos (completion-node.left node)))
               
               ((char> (aref str pos)
                       (completion-node.char node))
                (unless (completion-node.right node)
                  (setf (completion-node.right node)
                        (completion-node (aref str pos))))
                (completion-tree-add-node pos (completion-node.right node)))
               
               (t (if (= (1+ pos) (length str))
                      (setf (completion-node.endp node) t)
                      (progn
                        (unless (completion-node.middle node)
                          (setf (completion-node.middle node)
                                (completion-node (aref str (1+ pos)))))
                        (completion-tree-add-node (1+ pos)
                                                  (completion-node.middle node))))))))
    (unless (completion-tree.root tree)
      (setf (completion-tree.root tree)
            (completion-node (aref str 0))))
    
    (completion-tree-add-node 0 (completion-tree.root tree))
    
    tree))

(defun completion-tree-add* (tree &rest strings)
  "Add a list of strings to TREE. The strings are suffled to help
balance the tree."
  (dolist (s (qtl:shuffle strings) tree)
    (completion-tree-add tree s)))

(defun completion-tree-contains-p (tree str)
  "Check if TREE contains the word STR."
  (do ((pos 0)
       (node (completion-tree.root tree)))  
      ((null node))                     ; While NODE is not null...

    (cond
      ((char< (aref str pos)
              (completion-node.char node))
       (setf node (completion-node.left node)))
      
      ((char> (aref str pos)
              (completion-node.char node))
       (setf node (completion-node.right node)))
      
      (t (if (= (incf pos) (length str))
             (return-from completion-tree-contains-p (completion-node.endp node))
             (setf node (completion-node.middle node))))))
  
  nil)                                  ; Return NIL otherwise...

(defun completion-node-completions (node)
  "Walk the children of NODE to find all completions."
  (labels ((stringify-cat (x y)
             (concatenate 'string
                          (string x)
                          (string y)))
           
           ;; Heavily non-tail recursive. Can we simplify?
           (compute-node-completions (node prefix)
             (when node
               (let* ((cstr   (string (completion-node.char node)))
                      (end?   (completion-node.endp node))
                      (left   (completion-node.left node))
                      (middle (completion-node.middle node))
                      (right  (completion-node.right node)))
                 
                 (append (and end? (list (stringify-cat prefix cstr)))
                         (compute-node-completions middle (stringify-cat prefix cstr))
                         (compute-node-completions left prefix)
                         (compute-node-completions right prefix))))))
    (compute-node-completions node "")))

;;;;;;;;;;;;;;;;;; Traveling along completion nodes ;;;;;;;;;;;;;;;;;;

(defgeneric completion-node-travel (node item)
  (:documentation "Travel to the next node from NODE along the
  branch(es) specified by ITEM."))

(defmethod completion-node-travel ((node completion-node) (item character))
  (cond
    ((null node) nil)
    
    ((char< item (completion-node.char node))
     (travel-completion-node-by-character (completion-node.left node) item))
    
    ((char> item (completion-node.char node))
     (travel-completion-node-by-character (completion-node.right node) item))
    
    (t (completion-node.middle node))))

(defmethod completion-node-travel ((node completion-node) (item list))
  (cond
    ((null item) node)
    ((null node) nil)
    (t (completion-node-travel (completion-node-travel node (car item))
                               (cdr item)))))

(defmethod completion-node-travel ((node completion-node) (item string))
  (completion-node-travel node (qtl:explode item)))

;;;;;;;;;;;;;;;;;;;;;;; Completion computation ;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric compute-completions (node-or-tree item)
  (:documentation "Compute the completions of of ITEM given a node or
  tree NODE-OR-TREE."))

(defmethod compute-completions ((node completion-node) item)
  (completion-node-completions (completion-node-travel node item)))

(defmethod compute-completions ((tree completion-tree) item)
  (completion-node-completions
   (completion-node-travel (completion-tree.root tree) item)))

