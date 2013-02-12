;;;; defdata.lisp
;;;; Copyright (c) 2013 Robert Smith

(defvar *field-suffix* "FIELD-")

(flet ((unwrap-singletons (list)
         (mapcar #'(lambda (x)
                     (if (and (listp x)
                              (= 1 (length x)))
                         (first x)
                         x))
                 list))
       
       (ensure-car (obj)
         (if (consp obj)
             (car obj)
             obj))
       
       (gen-names (n)
         (loop :for i :below n
               :collect (make-symbol (format nil "~A~D" *field-suffix* i)))))
  
  (defmacro defdata (adt-name &body constructors)
    `(progn
       ;; Define the data type.
       (defstruct (,adt-name (:constructor nil)))
       
       ;; Define each of the field constructors.
       ,@(loop :for ctor :in (unwrap-singletons constructors)
               :collect
               (etypecase ctor
                 ;; Nullary constructor
                 (symbol `(defstruct (,ctor
                                      (:include ,adt-name)
                                      (:constructor ,ctor))))
                 (list (let* ((ctor-name (first ctor))
                              (field-types (rest ctor))
                              (field-names (gen-names (length field-types))))
                         `(defstruct (,ctor-name
                                      (:include ,adt-name)
                                      (:constructor ,ctor-name (,@field-names)))
                            ,@(mapcar #'(lambda (name type)
                                          `(,name (error "Unspecified field.")
                                                  :type ,type))
                               field-names
                               field-types))))))
       
       ',adt-name)))

(labels ((field (name n)
           (intern (format nil "~A-~A~D" name *field-suffix* n)
                   (symbol-package name)))
         
         (wild? (s)
           (string= "_" (symbol-name s))))

  ;; Setter
  (defmacro set-data (obj (name &rest new-values))
    (let ((once (gensym "ONCE")))
      `(let ((,once ,obj))
         (psetf
          ,@(loop :for i :from 0
                  :for x :in new-values
                  :when (not (wild? x))
                    :append (list `(,(field name i) ,once)
                                  x))))))
  
  ;; Destructuring
  (defmacro with-data ((name &rest vars) obj &body body)
    (let* ((once (gensym "ONCE-"))
           (bindings (loop :for i :from 0
                           :for v :in vars
                           :when (not (wild? v))
                             :collect `(,v (,(field name i)
                                            ,once)))))
      `(let ((,once ,obj))
         (let (,@bindings)
           ,@body)))))

;; TODO
;; (defmacro match (obj &body clauses))
