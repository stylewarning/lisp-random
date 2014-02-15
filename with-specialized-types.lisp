;;;; with-specialized-types.lisp
;;;;
;;;; Copyright (c) 2013-2014 Robert Smith

;;; We trade space for speed for space here. Compilers like SBCL can
;;; specialize code paths in each branch of the ETYPECASE since it
;;; knows the type of VAL. Maybe useful for numerical code.

(defmacro with-specialized-types (val types &body body)
  `(etypecase ,val
     ,@(loop :for ty :in types
             :collect (list* ty body))))
