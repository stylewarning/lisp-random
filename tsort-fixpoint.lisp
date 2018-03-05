;;;; tsort-fixpoint.lisp
;;;;
;;;; Copyright (c) 2018 Robert Smith

(defstruct graph
  vertices                              ; List of symbols
  edges                                 ; List of (A . B) representing
                                        ; the edge A -> B.
  )

(defun have-parents (g)
  "Which edges of G have parents?"
  (remove-duplicates
   (mapcar #'cdr (graph-edges g))))

(defun no-parents (g)
  "Which edges of G don't have parents?"
  (set-difference (graph-vertices g)
                  (have-parents g)))

(defun remove-vertices (g vertex-list)
  "Remove the vertices (and consequently, edges) of G that are in the list VERTEX-LIST."
  (make-graph
   :vertices (set-difference (graph-vertices g)
                             vertex-list)
   :edges (remove-if (lambda (edge)
                       (let ((from (car edge))
                             (to (cdr edge)))
                         (or (member from vertex-list)
                             (member to vertex-list))))
                     (graph-edges g))))

(defun fixpoint (f &rest args)
  "Compute a fixed point of F directly, starting with ARGS."
  (let ((results (multiple-value-list (apply f args))))
    (if (equalp results args)
        (values-list results)
        (apply #'fixpoint f results))))

(defun tsort-driver (g list)
  "A function such that when starting with a DAG G and an empty list, the fixed point will be an empty graph and the original graph topologically sorted."
  (if (null (graph-vertices g))
      (values g list)
      (let ((dont-have-parents (no-parents g)))
        (values (remove-vertices g dont-have-parents)
                (append list dont-have-parents)))))

(defun tsort (g)
  "Topologically sort the graph G."
  (nth-value 1 (fixpoint #'tsort-driver g '())))


;;; Tests

(defun shuffle-graph (g)
  (make-graph
   :vertices (alexandria:shuffle
              (graph-vertices g))
   :edges (alexandria:shuffle
           (graph-edges g))))


(defvar *example-one*
  (make-graph :vertices '(a b c d)
              :edges '((a . b)
                       (a . c)
                       (b . d)
                       (c . d))))

(defvar *example-two*
  (make-graph :vertices '(a b c d e)
              :edges '((a . b)
                       (a . c)
                       (b . d)
                       (c . d)
                       (e . c))))
