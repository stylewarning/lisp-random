(declaim (optimize speed
                   (safety 0)
                   (debug 0)))

(defstruct (ternary-node (:conc-name ternary-node.)
                         (:print-function ternary-node-printer))
  (char   #\nul :type base-char :read-only t)
  (end-p  nil   :type boolean)
  (left   nil   :type (or null ternary-node))
  (middle nil   :type (or null ternary-node))
  (right  nil   :type (or null ternary-node)))

(defun ternary-node-printer (obj stream depth)
  "Le printer for ternary nodes."
  (declare (type (unsigned-byte 25) depth))
  (let ((indent (make-string (* 2 depth) :initial-element #\space)))
    (when obj
      (format stream "~C~:[~;*~]"
              (ternary-node.char obj)
              (ternary-node.end-p obj))
      (when (ternary-node.left obj)
        (format stream "~&~A< " indent)
        (ternary-node-printer (ternary-node.left obj) stream (1+ depth)))
      
      (when (ternary-node.middle obj)
        (format stream "~&~A| " indent)
        (ternary-node-printer (ternary-node.middle obj) stream (1+ depth)))
      
      (when (ternary-node.right obj)
        (format stream "~&~A> " indent)
        (ternary-node-printer (ternary-node.right obj) stream (1+ depth))))))

(defun ternary-node (char &optional end-p)
  "Make a ternary node."
  (make-ternary-node :end-p end-p
                     :char char))

(defstruct (ternary-tree (:conc-name ternary-tree.)
                         (:print-function ternary-tree-printer))
  (root nil :type (or null ternary-node)))

(defun ternary-tree-printer (obj stream depth)
  "Le printer for ternary trees."
  (declare (type (unsigned-byte 25) depth))
  (when (ternary-tree.root obj)
    (ternary-node-printer (ternary-tree.root obj)
                          stream
                          depth)))

(defun ternary-tree-add (tree str)
  "Add a string STR to the ternary tree TREE."
  (labels ((ternary-tree-add-node (pos node)
             (declare (type (unsigned-byte 25) pos))
             (cond 
               ((char< (aref str pos)
                       (ternary-node.char node))
                (unless (ternary-node.left node)
                  (setf (ternary-node.left node)
                        (ternary-node (aref str pos))))
                (ternary-tree-add-node pos (ternary-node.left node)))
               
               ((char> (aref str pos)
                       (ternary-node.char node))
                (unless (ternary-node.right node)
                  (setf (ternary-node.right node)
                        (ternary-node (aref str pos))))
                (ternary-tree-add-node pos (ternary-node.right node)))
               
               (t (if (= (1+ pos) (length str))
                      (setf (ternary-node.end-p node) t)
                      (progn
                        (unless (ternary-node.middle node)
                          (setf (ternary-node.middle node)
                                (ternary-node (aref str (1+ pos)))))
                        (ternary-tree-add-node (1+ pos)
                                               (ternary-node.middle node))))))))
    (unless (ternary-tree.root tree)
      (setf (ternary-tree.root tree)
            (ternary-node (aref str 0))))
    
    (ternary-tree-add-node 0 (ternary-tree.root tree))
    
    tree))

;;;;;;;;;;;;;;;;;;;;;;;; Auxiliary Functions ;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun random-between (a b)
    "Generate a random integer between A and B, inclusive."
    (assert (>= b a))
    (if (= a b)
        a
        (+ a (random (- (1+ b) a)))))

  (defun shuffle (list &optional (parity :any))
    "Shuffle the permutation vector VECTOR with specified parity
  PARITY. PARITY may be

    * :ANY  for any permutation
    * :EVEN for only even permutations
    * :ODD  for only odd permutations"
    
    (assert (member parity '(:any :even :odd)))
    (let ((vector (make-array (length list)
                              :initial-contents list)))
      (let ((n (length vector))
            (any? (eql parity :any)))
        (loop :for i :below (if any? n (1- n))
              :for r := (random-between i (1- n))
              :when (/= i r)
              :do (progn
                    (rotatef (svref vector i)
                             (svref vector r))
                    (unless any?
                      (rotatef (svref vector (- n 1))
                               (svref vector (- n 2)))))
              :finally (progn
                         (when (eql parity :odd)
                           (rotatef (svref vector 0)
                                    (svref vector 1)))
                         (return (concatenate 'list vector))))))))

(defun ternary-tree-add* (tree &rest strings)
  "Add a list of strings to TREE. The strings are suffled to help
balance the tree."
  (dolist (s (shuffle strings) tree)
    (ternary-tree-add tree s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Testing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +word-list+
  (shuffle (with-open-file (s "/Users/quad/words")
             (loop :for word := (read-line s nil nil)
                   :while word
                   :collect word))))

(defparameter *tree* nil)

(defun test-ternary-tree (&optional (word-list +word-list+))
  (declare (optimize speed))
  (let ((tree (make-ternary-tree)))
    (dolist (w word-list nil)
      (ternary-tree-add tree w))
    (setf *tree* tree)
    nil))