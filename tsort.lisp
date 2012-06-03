;;;; tsort.lisp
;;;; Copyright (c) 2012 Robert Smith

;;; A directed acyclic graph should be specified as follows:
;;;
;;;    ((<node> <dependency> ...)
;;;     ...
;;;    )

(defun list-sinks (dag)
  "Find the sinks in DAG."
  (loop :for (node . deps) :in dag
        :when (null deps)
          :collect node))

(defun tsort (graph)
  "Topologically sort GRAPH. An error is signalled if it is not a
directed acyclic graph."
  (let* ((sorted nil)                   ; Final sorted list.
         (sinks  (list-sinks graph))    ; Sinks in the graph.
         (dag    (copy-tree graph)))    ; Copy of the graph that we will mutate.
    (loop :while (not (null sinks))
          :do (progn
                ;; Remove the sinks.
                (setf dag (delete-if (lambda (x) (null (cdr x))) dag))
                
                ;; Get the next sink.
                (let ((sink (pop sinks)))
                  
                  ;; Add it to the sorted list.
                  (push sink sorted)
                  
                  ;; For every node/neighborhood...
                  (dolist (node dag)
                    
                    ;; Remove the sink from the dependencies if any
                    ;; exist.
                    (setf (cdr node) (delete sink (cdr node)))
                    
                    ;; If we have no more dependencies, add it to the
                    ;; sinks.
                    (when (null (cdr node))
                      (push (car node) sinks)))))
          :finally (return (if (null dag)
                               ;; Our DAG is empty. We're good!
                               (nreverse sorted)

                               ;; Our DAG isn't empty but has no
                               ;; sinks. It must be cyclic!
                               (error "Cannot sort a cyclic graph. ~
                                       The cycles are ~S." dag))))))
