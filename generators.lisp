;;;; generators.lisp
;;;; Copyright (c) 2012 Robert Smith

;;;; Some trivial generators


;;; Error conditions

(define-condition generator-exhausted (error) ()
  (:report "Generator has been exhausted."))

(declaim (inline exhausted))
(defun exhausted ()
  (error 'generator-exhausted))

;;; Iterators

(defun xrange (a b &optional (step 1))
  (let ((x a))
    (lambda ()
      (if (> x b)
          (exhausted)
          (prog1 x
            (incf x step))))))

(defun each (list)
  (lambda ()
    (if (null list)
        (exhausted)
        (prog1 (car items)
          (setf items (cdr items))))))

(defun each-of (&rest items)
  (each items))
