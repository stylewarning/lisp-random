(declaim (optimize speed (safety 0) (debug 0) (space 0)))
(defun collect ()
  (let* ((collection (cons nil nil))
         (tail collection))
    (declare (type cons collection tail))
    (lambda (&rest args)
      (declare (dynamic-extent args))
      (the list
           (dolist (arg args (cdr collection))
             (let ((new-tail (cons arg nil)))
               (declare (type cons new-tail))
               (setf (cdr tail) new-tail)
               (setf tail new-tail)))))))
