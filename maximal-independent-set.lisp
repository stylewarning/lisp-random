;;;; maximal-independent-set.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(defun maximal-independent-set (graph neighborp)
  "Compute a maximal independent set of GRAPH (a list of vertices) where NEIGHBORP is a binary function to test whether two nodes are neighbors."
  (loop :with s := nil
        :until (endp graph)
        :for v := (pop graph)
        :do ;; add v to indep. set
            (push v s)
            ;; remove its neighbors
            (setf graph (remove-if (lambda (w) (funcall neighborp v w)) graph))
        :finally (return s)))


;;; Driver for asjackson's problem:
;;;
;;; "Given a set of points and a distance d, find the largest subset
;;; of points which are all >= d apart."

(defun distance (p q)
  (sqrt (loop :for p_i :in p
              :for q_i :in q
              :sum (expt (- p_i q_i) 2))))

(defun asjackson (points min-distance)
  (maximal-independent-set points (lambda (u v) (> min-distance (distance u v)))))
