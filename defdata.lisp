;;;; defdata.lisp
;;;; Copyright (c) 2013 Robert Smith
;;;;
;;;; See defdata-examples.lisp for examples.


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *field-prefix* "%")
  (defvar *constructors* (make-hash-table))
  
  (defun get-constructors (adt)
    (gethash adt *constructors*))
  
  (defun set-constructors (adt constructors)
    (setf (gethash adt *constructors*)
          constructors))
  
  (defmacro define-constant (name value &optional doc)
    `(defconstant ,name (if (boundp ',name)
                            (symbol-value ',name)
                            ,value)
       ,@(when doc (list doc))))
  
  (defun wild? (s)
    (and (symbolp s)
         (string= "_" (symbol-name s))))
  
  (defun ensure-list (x)
    (if (listp x)
        x
        (list x)))
  
  (defun ensure-car (x)
    (if (consp x)
        (car x)
        x))
  
  (defun internal (s)
    (intern (format nil "%~A" s))))


;;; DEFDATA definition
(flet ((unwrap-singletons (list)
         (mapcar #'(lambda (x)
                     (if (and (listp x)
                              (= 1 (length x)))
                         (first x)
                         x))
                 list))
       
       (gen-names (n)
         (loop :for i :below n
               :collect (make-symbol (format nil "~A~D" *field-prefix* i)))))
  
  (defmacro defdata (adt-name &body constructors)
    
    ;; Add constructors to the database.
    (set-constructors adt-name
                      (mapcar #'ensure-car constructors))
    
    ;; Define everything.
    `(progn
       ;; Define the data type.
       (defstruct (,adt-name (:constructor nil)))
       
       ;; Define each of the field constructors.
       ,@(loop :for ctor :in (unwrap-singletons constructors)
               :collect
               (etypecase ctor
                 ;; Nullary constructor
                 (symbol `(progn
                            (defstruct (,ctor
                                        (:include ,adt-name)
                                        (:constructor ,(internal ctor))))
                            (define-constant ,ctor (,(internal ctor)))
                            (fmakunbound ',(internal ctor))))
                 
                 ;; N-ary constructors
                 (list (let* ((ctor-name (first ctor))
                              (field-types (rest ctor))
                              (field-names (gen-names (length field-types))))
                         `(defstruct (,ctor-name
                                      (:include ,adt-name)
                                      (:constructor ,ctor-name (,@field-names))
                                      (:conc-name ,ctor-name))
                            ,@(mapcar #'(lambda (name type)
                                          `(,name (error "Unspecified field.")
                                                  :type ,type))
                               field-names
                               field-types))))))
       
       ;; Return the type name
       ',adt-name)))

(flet ((field (name n)
           (intern (format nil "~A~A~D" name *field-prefix* n)
                   (symbol-package name))))

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
         (declare (ignorable ,once))
         (let (,@bindings)
           ,@body)))))

(defmacro match (adt obj &body clauses)
  (assert (symbolp adt)
          (adt)
          "MATCH requires a symbol for the first argument. Given ~S."
          adt)
  
  (let ((ctors (get-constructors adt))
        (types (mapcar (lambda (clause)
                         (ensure-car (car clause)))
                       clauses))
        (once (gensym "ONCE-")))
    
    ;; Check for match exhaustiveness.
    (unless (some #'wild? types)
      (let ((diff (set-difference ctors types)))
        (when diff
          (warn "Non-exhaustive match. Missing cases: ~S" diff))))
    
    ;; Generate the matching code.
    `(let ((,once ,obj))
       (etypecase ,obj
         ,@(loop :for (bindings . body) :in clauses
                 :collect (let ((type (ensure-car bindings)))
                            (if (wild? type)
                                `(t ,@body)
                                `(,type 
                                  (with-data ,(ensure-list bindings)
                                             ,once
                                    ,@body)))))))))
