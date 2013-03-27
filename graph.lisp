;;;; graph.lisp
;;;; Copyright (c) 2013 Robert Smith

;;;; An assortment of operations on directed multigraphs.

;;;;;;;;;;;;;;;;;;;;;;;; Graph Representation ;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct vertex
  info
  edges)

(defstruct edge
  info
  from
  to)

(defstruct (graph (:print-function
                   (lambda (obj str dep)
                     (declare (ignore dep))
                     (print-unreadable-object (obj str :type t :identity t)))))
  vertices
  edges)


;;;;;;;;;;;;;;;;;;;;;;;;; Graph Construction ;;;;;;;;;;;;;;;;;;;;;;;;;

(defun same-name (s1 s2)
  "Do the symbols S1 and S2 have the same name?"
  (string= (symbol-name s1)
           (symbol-name s2)))

(defun add-vertex (graph vertex)
  "Add the vertex VERTEX to the graph GRAPH."
  (pushnew vertex (graph-vertices graph) :test #'eq))

;; This is extremely inefficient unfortunately.
(defun add-edge (graph edge)
  "Add the edge EDGE to the graph GRAPH. Insert new vertexes as
necessary."
  (pushnew edge (graph-edges graph) :test #'eq)
  (add-vertex graph (edge-from edge))
  (add-vertex graph (edge-to edge))
  graph)

(defun construct-graph (adjacencies)
  (let ((graph (make-graph))
        (dict (make-hash-table)))
    (flet ((find-or-make-vertex (id)
             (or (gethash id dict)
                 (setf (gethash id dict)
                       (make-vertex :info id)))))
      (loop :for (from direction to . edge-info) :in adjacencies
            :do (let* ((from-vertex (find-or-make-vertex from))
                       (to-vertex   (find-or-make-vertex to))
                       (a->b (make-edge :from from-vertex
                                        :to to-vertex
                                        :info (car edge-info)))
                       (b->a (make-edge :from to-vertex
                                        :to from-vertex
                                        :info (car edge-info))))
                  (cond
                    ((same-name '-- direction) (progn
                                                 (add-edge graph a->b)
                                                 (add-edge graph b->a)))
                    ((same-name '-> direction) (add-edge graph a->b))
                    ((same-name '<- direction) (add-edge graph b->a))
                    (t (error "Invalid direction. Given: `~S'" direction))))
            :finally (return graph)))))


;;;;;;;;;;;;;;;;;;;;;;;;; Vertex Operations ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vertex-neighbors (vertex)
  "Compute the neighbors of the vertex VERTEX."
  (delete-duplicates (mapcar #'edge-to (vertex-edges vertex))
                     :test #'eq))

(defun adjacentp (vertex-1 vertex-2)
  "Is VERTEX-2 adjacent to VERTEX-1?"
  (find vertex-2 (vertex-neighbors vertex-1) :test #'eq))

(defun vertex-degree (vertex)
  "Compute the degree of the vertex VERTEX."
  (length (vertex-neighbors vertex)))


;;;;;;;;;;;;;;;;;;;;;;;;;; Edge Operations ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun edge-loop-p (edge)
  "Is the edge EDGE a loop?"
  (eq (edge-from edge)
      (edge-to edge)))


;;;;;;;;;;;;;;;;;;;;;;;;;; Graph Operations ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun graph-order (graph)
  "The order of a graph GRAPH, equal to the number of vertices."
  (length (graph-vertices graph)))

(defun graph-size (graph)
  "The size of a graph GRAPH, equal to the number of edges."
  (length (graph-edges graph)))

(defun map-vertices (f graph)
  "Map the unary function F across all vertices of GRAPH."
  (mapcar f (graph-vertices graph)))

(defun for-each-vertex (f graph)
  "For each vertex of GRAPH, call the unary function F."
  (mapc f (graph-vertices graph)))

(defun map-edges (f graph)
  "Map the unary function F across all edges of GRAPH."
  (mapcar f (graph-edges graph)))

(defun for-each-edge (f graph)
  "For each edge of GRAPH, call the unary function F."
  (mapc f (graph-edges graph)))


;;;;;;;;;;;;;;;;;;;;;;;;;;; Special Graphs ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun complete-graph (order)
  (let ((graph (make-graph))
        (vertices (loop :repeat order :collect (make-vertex))))
    (maplist (lambda (v-vx)
               (let ((from (car v-vx))
                     (to   (cdr v-vx)))
                 (mapc (lambda (vertex)
                         (add-edge graph (make-edge :from from
                                                    :to vertex))
                         (add-edge graph (make-edge :from vertex
                                                    :to from)))
                       to)))
             vertices)
    graph))

;;; MAKE C++

(defun graph->c++ (graph)
  (labels ((vertex-name (vertex)
             (symbol-name (vertex-info vertex)))
           
           (output-vertex (vertex)
             (let ((name (vertex-name vertex)))
               (if (string= name "NIL")
                   (format t "NFA ~A(true);~%" name)
                   (format t "NFA ~A;~%" name))))
           
           (output-edge (edge)
             (let ((from (vertex-name (edge-from edge)))
                   (to   (vertex-name (edge-to edge)))
                   (char (edge-info edge)))
               (if (null char)
                   (format t "~A.addEdge(NFA::Epsilon, &~A);~%" from to)
                   (format t "~A.addEdge('~C', &~A);~%" from char to)))))
    (for-each-vertex #'output-vertex graph)
    (for-each-edge #'output-edge graph)
    t))
