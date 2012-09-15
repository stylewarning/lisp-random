;;;; generators.lisp
;;;; Copyright (c) 2012 Robert Smith

;;;; Some trivial generators


;;; Error conditions

(define-condition generator-exhausted (error) ()
  (:report "Generator has been exhausted."))

(declaim (inline exhausted))
(defun exhausted ()
  (restart-case (error 'generator-exhausted)
    (return-nil ()
      :report "Return NIL."
      nil)
    
    (specify-value (value)
      :report "Specify a value to return."
      :interactive (lambda ()
                     (format t "Enter a value (unevaluated): ")
                     (list (read)))
      value)))

;;; Generate all values of a generator

(defun collect (gen)
  "Collect all of the values of the generator GEN into a list."
  (let ((collected nil))
    (loop
      (handler-case (push (funcall gen)
                          collected)
        (generator-exhausted ()
          (return (nreverse collected)))))))

(defun take (n gen)
  "Collect at most N values of the generator GEN into a list."
  (let ((collected nil))
    (loop :repeat n
          :do (handler-case (push (funcall gen)
                                  collected)
                (generator-exhausted ()
                  (return (nreverse collected))))
          :finally (return (nreverse collected)))))

;;; Iterators

(defun xrange (a b &optional (step 1))
  "Return a generator that returns values from A to B, with a step
size of STEP."
  (let ((x a))
    (lambda ()
      (if (> x b)
          (exhausted)
          (prog1 x
            (incf x step))))))

(defun each (list)
  "Return a generator that iterates through the list LIST."
  (lambda ()
    (if (null list)
        (exhausted)
        (prog1 (car list)
          (setf list (cdr list))))))

(defun each-of (&rest items)
  "Return a generator that iterates through each of the arguments
ITEMS."
  (each items))
