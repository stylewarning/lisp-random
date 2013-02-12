(defmacro defdata (adt-name &body constructors)
  `(progn
     (defstruct ,adt-name)
     ,@(loop :for ctor :in constructors
             :collect (etypecase ctor
                        (symbol `(defstruct (,ctor
                                             (:include ,adt-name)
                                             (:constructor ,ctor))))
                        (list (let* ((ctor-name (first ctor))
                                     (field-types (rest ctor))
                                     (field-names (mapcar
                                                   (lambda (x)
                                                     (declare (ignore x))
                                                     (gensym (symbol-name ctor-name)))
                                                   field-types)))
                                `(defstruct (,ctor-name
                                             (:include ,adt-name)
                                             (:constructor ,ctor-name (,@field-names)))
                                   ,@(mapcar #'(lambda (name type)
                                                 `(,name (error "Unspecified field.")
                                                         :type ,type))
                                      field-names
                                      field-types))))))
     ',adt-name))
