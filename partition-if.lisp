;;;; partition-if.lisp
;;;; Copyright (c) 2013 Robert Smith

;;;; Implementations of PARTITION-IF and EQUIVALENCE-CLASSES.

#+(or)
(defun partition-if (f list)
  (loop :for x :in list
        :if (funcall f x)
          :collect x :into yes
        :else
          :collect x :into no
        :finally (return (list yes no))))

(defun partition-if (f seq)
  "Partition the sequence SEQ into a list of elements satisfying the
predicate F and a list of elements who do not."
  (let ((yes nil)
        (no nil))
    (map nil
         (lambda (x)
           (if (funcall f x)
               (push x yes)
               (push x no)))
         seq)
    (list (nreverse yes)
          (nreverse no))))

(defun equivalence-classes (equiv seq)
  "Partition the sequence SEQ into a list of equivalence classes
according to the equivalence relation EQUIV."
  (let ((half-length (floor (length seq) 2))
        (classes nil))
    (labels ((find-equivalence-class (x)
               (find-if (lambda (class)
                          (funcall equiv (aref class 0) x))
                        classes))
             
             (new-class (x)
               (make-array half-length
                           :initial-element x
                           :adjustable t
                           :fill-pointer 1))
             
             (add-to-class (x)
               (let ((class (find-equivalence-class x)))
                 (if class
                     (vector-push-extend x class)
                     (push (new-class x) classes)))))
      
      ;; Partition into equivalence classes.
      (map nil #'add-to-class seq)
      
      ;; Return the classes.
      classes)))
