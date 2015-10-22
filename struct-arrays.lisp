;;;; struct-arrays.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct slot-entry
    initial-value-form
    type
    upgraded-type
    array-slot
    offset
    record-length
    accessor-name))

(defmacro define-struct-array (name &body slots &environment env)
  (let (
        ;; Map from slot name -> SLOT-ENTRY
        (slot-table (make-hash-table :test 'eq))
        ;; Map from upgraded type -> list of SLOT-ENTRY
        (type-table (make-hash-table :test 'equal))
        ;; Map from array slot name -> list of SLOT-ENTRY
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
    (flet ((frob (k v)
             (declare (ignore k))
             (let ((array-slot-name (gensym (symbol-name name)))
                   (length (length v)))
               (loop :for offset :from 0
                     :for slot-name :in v
                     :for entry := (gethash slot-name slot-table)
                     :do (setf (slot-entry-array-slot entry) array-slot-name
                               (slot-entry-offset entry) offset
                               (slot-entry-record-length entry) length
                               (slot-entry-accessor-name entry)
                               (intern (concatenate 'string
                                                    (symbol-name name)
                                                    "-"
                                                    (symbol-name slot-name))))
                         (push entry (gethash array-slot-name array-table))))))
      (maphash #'frob type-table))

    ;; Generate code.
    (let ((%make-name (gensym (concatenate 'string "MAKE-" (symbol-name name))))
          (make-name (intern (concatenate 'string "MAKE-" (symbol-name name))))
          (array-slots (loop :for k :being :the :hash-keys :of array-table
                             :collect k))
          (num-elements (gensym "NUM-ELEMENTS-"))
          (keep-uninitialized (gensym "KEEP-UNINITIALIZED-"))
          (vec (gensym "VEC"))
          (i (gensym "I")))
      (labels ((getter/setter-for-entry (slot-name entry)
                 (declare (ignore slot-name))
                 (let ((name (slot-entry-accessor-name entry))
                       (accessor `(aref
                                   (slot-value obj ',(slot-entry-array-slot entry))
                                   (+ ,(slot-entry-offset entry)
                                      (* idx ,(slot-entry-record-length entry))))))
                   (list
                    `(declaim (inline ,name (setf ,name)))
                    `(defun ,name (obj idx)
                       ,accessor)
                    `(defun (setf ,name) (new-value obj idx)
                       (setf ,accessor new-value))
                    `(declaim (notinline ,name (setf ,name)))))))
        `(progn
           ;; Define the structure to hold the vectors.
           (defstruct (,name (:constructor ,%make-name ,array-slots))
             ,@(loop :for array-slot :in array-slots
                     :for type := (slot-entry-upgraded-type (first (gethash array-slot array-table)))
                     :collect `(,array-slot nil :type (simple-array ,type (*)))))

           ;; Define the getters.
           ,@(loop :for slot-name :being :the :hash-keys :of slot-table
                     :using (hash-value entry)
                   :append (getter/setter-for-entry slot-name entry))

           ;; Define the constructor.
           (defun ,make-name (,num-elements &key ((:keep-uninitialized ,keep-uninitialized) nil))
             (let ((,vec (,%make-name
                          ,@(loop :for array-slot :in array-slots
                                  :for type := (slot-entry-upgraded-type (first (gethash array-slot array-table)))
                                  :collect `(make-array ,num-elements :element-type (quote ,type))))))
               (unless ,keep-uninitialized
                 (dotimes (,i ,num-elements)
                   (psetf ,@(loop :for entry :being :the :hash-values :of slot-table
                                  :append `((,(slot-entry-accessor-name entry) ,vec ,i)
                                            ,(slot-entry-initial-value-form entry))))))
               ,vec))

           ;; Return the structure name.
           ',name)))))

#+example
(define-struct-array vertexes
  ;; Position
  (x 0.0 single-float)
  (y 0.0 single-float)
  (z 0.0 single-float)
  ;; Color
  (r 0 (integer 0 255))
  (g 0 (integer 0 255))
  (b 0 (integer 0 255)))
