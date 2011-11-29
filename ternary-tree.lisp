(defstruct (ternary-node (:conc-name ternary-node.)
                         #+#:ig
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
        (format stream "~&~A> " indent)
        (ternary-node-printer (ternary-node.left obj) stream (1+ depth)))
      
      (when (ternary-node.middle obj)
        (format stream "~&~A| " indent)
        (ternary-node-printer (ternary-node.middle obj) stream (1+ depth)))

      (when (ternary-node.right obj)
        (format stream "~&~A< " indent)
        (ternary-node-printer (ternary-node.right obj) stream (1+ depth))))))

(defun ternary-node (char &optional end-p)
  "Make a ternary node."
  (make-ternary-node :end-p end-p
                     :char char))

(defstruct (ternary-tree (:conc-name ternary-tree.)
                         #+#:ig
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

(defun ternary-tree-contains-p (tree str)
  (do ((pos 0)
       (node (ternary-tree.root tree)))  
      ((null node))                       ; While NODE is not null...

    (cond
      ((char< (aref str pos)
              (ternary-node.char node))
       (setf node (ternary-node.left node)))
      
      ((char> (aref str pos)
              (ternary-node.char node))
       (setf node (ternary-node.right node)))
      
      (t (progn
           (incf pos)
           (when (= pos (length str))
             (return-from ternary-tree-contains-p (ternary-node.end-p node)))
           (setf node (ternary-node.middle node))))))
  
  nil)                               ; Return NIL otherwise...

(defun ternary-node-completions (node)
  (labels ((strcat (x y)
             (concatenate 'string
                          (string x)
                          (string y)))
           
           (compute-completions (node prefix)
             (when node
               (let* ((cstr   (string (ternary-node.char node)))
                      (end?   (ternary-node.end-p node))
                      (left   (ternary-node.left node))
                      (middle (ternary-node.middle node))
                      (right  (ternary-node.right node)))
                 
                 (append (and end? (list (strcat prefix cstr)))
                         (compute-completions middle (strcat prefix cstr))
                         (compute-completions left prefix)
                         (compute-completions right prefix))))))
   
    (compute-completions node "")))

(defun travel-node (node char)
  (assert (characterp char))
  
  (cond
    ((null node) nil)
    
    ((char< char (ternary-node.char node))
     (travel-node (ternary-node.left node) char))
    
    ((char> char (ternary-node.char node))
     (travel-node (ternary-node.right node) char))
    
    (t (ternary-node.middle node))))

(defun travel-node-by-string (node string)
  (labels ((string-to-list (string)
             (concatenate 'list string))
           
           (travel-node-by-list (node list)
             (cond
               ((null list) node)
               ((null node) nil)
               (t (travel-node-by-list (travel-node node (car list))
                                       (cdr list))))))
    (travel-node-by-list node (string-to-list string))))

(defun travel-and-complete (node char)
  (ternary-node-completions (travel-node node char)))

(defun completions (tree string)
  (ternary-node-completions
   (travel-node-by-string
    (ternary-tree.root tree)
    string)))

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

(defun get-word-list ()
  (with-open-file (s "/Users/quad/words")
    (loop :for word := (read-line s nil nil)
          :while word
          :collect word)))

(defun test-ternary-tree ()
  (declare (optimize speed))
  (let ((tree (make-ternary-tree)))
    (dolist (w (get-word-list) nil)
      (ternary-tree-add tree w))))