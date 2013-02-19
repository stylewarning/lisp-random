;;;; locative.lisp
;;;; Copyright (c) 2013 Robert Smith

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
;;; function can take multiple values.
(defsetf dereference (locative) (new-val)
  `(funcall (locative-writer ,locative) ,new-val))
