;;;; specialize-type.lisp
;;;; Copyright (c) 2013 Robert Smith

(defmacro with-specialized-types (val types &body body)
  `(etypecase ,val
     ,@(loop :for ty :in types
             :collect (list* ty body))))
