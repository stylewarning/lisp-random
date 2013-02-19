;;;; locative.lisp
;;;; Copyright (c) 2013 Robert Smith
;;;;
;;;; This is an example implementation of locatives by using closures
;;;; and GET-SETF-EXPANSION.
;;;;
;;;; Locatives were like C pointers for the lisp machine, but were
;;;; GC-safe. They were also efficiently represented, and specially
;;;; handled.

;;; Example usage:
;;;
;;; CL-USER> (let* ((x (make-array 5 :initial-element 0))
;;;                 (l (locative-for (aref x 2))))
;;;            (setf (dereference l) 5)
;;;            (list l
;;;                  x
;;;                  (dereference l)))
;;;
;;; (#<LOCATIVE {100612B943}> #(0 0 5 0 0) 5)


(defstruct (locative (:predicate locativep)
                     (:constructor %make-locative)
                     (:print-function
                      (lambda (obj stream depth)
                        (declare (ignore depth))
                        (print-unreadable-object (obj stream :type t
                                                             :identity t)))))
  (reader (error "Must provide reader function.") :type function
                                                  :read-only t)
  (writer (error "Must provide writer function.") :type function
                                                  :read-only t))

(defmacro locative-for (place &environment env)
  "Return a locative for PLACE in the environment ENV."
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
     (get-setf-expansion place env)
  `(let* ,(mapcar #'list vars vals)
     (%make-locative :reader (lambda () ,reader-form)
                     :writer (lambda ,store-vars ,writer-form)))))

(defun dereference (locative)
  "Return a value that a locative LOCATIVE points to."
  (funcall (locative-reader locative)))

;;; This DEFSETF is actually incorrect. It is possible that the writer
;;; function can take multiple values. To use the full functionality,
;;; one must simply use LOCATIVE-WRITER.
(defsetf dereference (locative) (new-val)
  `(funcall (locative-writer ,locative) ,new-val))
