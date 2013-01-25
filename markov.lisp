;;;; markov.lisp
;;;; Copyright (c) 2012 Robert Smith


;;; Utilities

(defun running-totals (vec)
  "Compute the successive totals of the vector VEC."
  (loop :for i :across vec
        :sum i :into sum
        :collecting sum :into totals
        :finally (return (coerce totals 'vector))))

(defun find-greatest-lower-bound (n seq)
  "Find the position of the maximal element X of the sequence SEQ such
  that N <= X."
  (position-if (lambda (x) (<= n x))
               seq))


;;; Distributions

(defstruct distribution
  length
  sum
  pdf
  cdf)

(defun compute-distribution (tallies)
  "Create a new distribution from the histogram TALLIES."
  (make-distribution
   :length (length tallies)
   :sum (reduce #'+ tallies :initial-value 0)
   :pdf tallies
   :cdf (running-totals tallies)))

(defun distributed-random (distribution)
  "Compute a random number between 0 and the length of DISTRIBUTION
according to the distribution DISTRIBUTION."
  (let ((r (1+ (random (distribution-sum distribution)))))
    (find-greatest-lower-bound r (distribution-cdf distribution))))


;;; Markov Nodes

(defstruct markov-node
  vertex
  edges
  traversal-distribution)

(defun create-markov-node (vertex edge-list)
  "Create a new Markov node with the edges in EDGE-LIST. The edges
should be of the form

    (VERTEX COUNT)

where count is the absolute count of that vertex."
  (make-markov-node
   :vertex vertex
   :edges (map 'vector #'first edge-list)
   :traversal-distribution (compute-distribution
                            (map 'vector #'second edge-list))))

(defun select-random-edge (markov-node)
  "Select a random edge of MARKOV-NODE to traverse."
  (aref (markov-node-edges markov-node)
        (distributed-random (markov-node-traversal-distribution markov-node))))


;;; Markov Chains

(defstruct markov-chain
  transition-table)

(defun create-markov-chain (markov-nodes)
  "Create a new Markov chain from the list of nodes MARKOV-NODES."
  (loop :with ht := (make-hash-table)
        :for node :in markov-nodes
        :do (setf (gethash (markov-node-vertex node) ht)
                  node)
        :finally (return (make-markov-chain :transition-table ht))))

(defun list-to-markov-chain (list)
  "Convert a list LIST to a Markov chain."
  (loop :for (vertex . edges) :in list
        :collect (create-markov-node vertex edges) :into nodes
        :finally (return (create-markov-chain nodes))))

(defun get-node (vertex markov-chain)
  "Get the node corresponding to VERTEX in the Markov chain
MARKOV-CHAIN."
  (gethash vertex (markov-chain-transition-table markov-chain)))

(defun random-transition (vertex markov-chain)
  "Return a new vertex by randomly traversing from VERTEX in the
  Markov chain MARKOV-CHAIN."
  (let ((node (get-node vertex markov-chain)))
    (select-random-edge node)))


;;; Copter's Markov Chain

(defvar *copter-chain*
  (list-to-markov-chain `((#\^ (#\:        39)
                               (#\D         1))
                          (#\: (#\D       168)
                               (#\:        12)
                               (#\Newline   8))
                          (#\D (#\:       137)
                               (#\D        14)
                               (#\Newline  32)))))

(defun copterize ()
  "Create a random Copter string."
  (with-output-to-string (stream)
    (loop :for edge :=    (random-transition #\^  *copter-chain*)
                    :then (random-transition edge *copter-chain*)
          :until (char= edge #\Newline)
          :do (write-char edge stream))))

(defun copterize-length ()
  "Compute the length of a random Copter string."
  (loop :for edge :=    (random-transition #\^  *copter-chain*)
                  :then (random-transition edge *copter-chain*)
        :until (char= edge #\Newline)
        :count t))
