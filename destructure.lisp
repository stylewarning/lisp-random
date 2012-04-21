;;;; destructure.lisp
;;;; Copyright (c) 2012 Robert Smith

(defgeneric destructure-to-list (thing)
  (:documentation "Generic function to convert THING into a list"))

(defmacro destructure ((&rest vars) thing &body body)
  `(destructuring-bind (,@vars) (destructure-to-list ,thing)
     (declare (ignorable ,@vars))
     ,@body))

(defmethod destructure-to-list ((n/d rational))
  (list (numerator n/d)
        (denominator n/d)))
