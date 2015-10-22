;;;; struct-arrays.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

(defun valid-type-p (purported-type)
  (handler-case (typep 0 purported-type)
    (error (c)
      (declare (ignore c))
      (return-from valid-type-p nil)))
  t)

(defstruct slot-entry
  initial-value-form
  type
  upgraded-type
  array-slot
  offset
  record-length)

(defmacro define-struct-array (name &body slots &environment env)
  (let ((slot-table (make-hash-table :test 'eq))
        (type-table (make-hash-table :test 'equal))
        (array-table (make-hash-table :test 'eq)))
    ;; Fill out the table with all the info about each slot.
    ;;
    ;; We iterate in reverse order because we collect in reverse order.
    (loop :for (slot-name initial-value-form type) :in (reverse slots)
          :do (let ((upgraded-type (upgraded-array-element-type type env)))
                (setf (gethash slot-name slot-table)
                      (make-slot-entry :initial-value-form initial-value-form
                                       :type type
                                       :upgraded-type upgraded-type))
                (push slot-name (gethash upgraded-type type-table))))
    
    ;; Fill out the offsets and generate array slot names.
    (flet ((foo (k v)
             (declare (ignore k))
             (let ((array-slot-name (gensym (symbol-name name)))
                   (length (length v)))
               (loop :for offset :from 0
                     :for slot-name :in v
                     :for entry := (gethash slot-name slot-table)
                     :do (setf (slot-entry-array-slot entry) array-slot-name
                               (slot-entry-offset entry) offset
                               (slot-entry-record-length entry) length)
                         (push entry (gethash array-slot-name array-table))))))
      (maphash #'foo type-table))

    ;; Generate code.
    (let ((%make-name (gensym (concatenate 'string "MAKE-" (symbol-name name))))
          (make-name (intern (concatenate 'string "MAKE-" (symbol-name name))))
          (array-slots (loop :for k :being :the :hash-keys :of array-table
                             :collect k))
          (num-elements (gensym "NUM-ELEMENTS"))
          (initialize (gensym "INITIALIZE"))
          (vec (gensym "VEC")))
      (labels ((getter/setter-for-entry (slot-name entry)
                 (let ((name (intern (concatenate 'string
                                                  (symbol-name name)
                                                  "-"
                                                  (symbol-name slot-name))))
                       (accessor `(svref
                                   (slot-value obj ',(slot-entry-array-slot entry))
                                   (+ ,(slot-entry-offset entry)
                                      (* idx ,(slot-entry-record-length entry))))))
                   (list
                    `(defun ,name (obj idx)
                       ,accessor)
                    `(defun (setf ,name) (new-value obj idx)
                       (setf ,accessor new-value))))))
        `(progn
           ;; Define the structure to hold the vectors.
           (defstruct (,name (:constructor ,%make-name ,array-slots))
             ,@(loop :for array-slot :in array-slots
                     :for type := (slot-entry-upgraded-type (first (gethash array-slot array-table)))
                     :collect `(,array-slot nil :type (simple-array ,type (*)))))

           ;; Define the constructor.
           (defun ,make-name (,num-elements &optional (,initialize t))
             (let ((,vec (,%make-name
                          ,@(loop :for array-slot :in array-slots
                                  :for type := (slot-entry-upgraded-type (first (gethash array-slot array-table)))
                                  :collect `(make-array ,num-elements :element-type (quote ,type))))))
               (when ,initialize
                 (warn "Not implemented"))
               ,vec))

           ;; Define the getters.
           ,@(loop :for slot-name :being :the :hash-keys :of slot-table
                     :using (hash-value entry)
                   :append (getter/setter-for-entry slot-name entry)))))))

#+example
(define-struct-array point
  (x 0 single-float)
  (y 0 single-float)
  (z 0 single-float))
