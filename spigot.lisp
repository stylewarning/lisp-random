;;;; spigot.lisp
;;;;
;;;; Author: Robert Smith

;;;; From http://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/spigot.pdf

(defun spigot (f)
  "Call F on all digits of pi."
  (labels
      ((g (q r s k n l)
         (declare (type integer q r s k n l))
         (cond
           ((< (- (+ (* 4 q) r) s)
               (* n s))
            (funcall f n)
            (g (* 10 q)
               (* 10 (- r (* n s)))
               s
               k
               (- (floor (* 10 (+ r (* 3 q)))
                         s)
                  (* 10 n))
               l))
           (t
            (g (* q k)
               (* l (+ r (* 2 q)))
               (* s l)
               (1+ k)
               (floor (+ (* q (+ 2 (* 7 k)))
                         (* r l))
                      (* s l))
               (+ l 2))))))
    (g 1 0 1 1 3 3)))
