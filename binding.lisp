;;;; binding.lisp
;;;;
;;;; Copyright (c) 2018 Robert Smith

;;; Lexical and dynamic binding as macros.

(defmacro let-lexical (bindings &body body)
  `((lambda ,(mapcar #'first bindings) ,@body) ,@(mapcar #'second bindings)))

(defvar *dynamic-values* (make-hash-table))
(defun lookup (name) (first (gethash name *dynamic-values*)))
(defun (setf lookup) (value name) (setf (first (gethash name *dynamic-values*)) value))

(defmacro defdynamic (name value)
  `(progn (define-symbol-macro ,name (lookup ',name))
          (setf (gethash ',name *dynamic-values*) (list ,value))
          ',name))

(defmacro let-dynamic (bindings &body body)
  (loop :for (name value) :in bindings
        :collect `(push ,value (gethash ',name *dynamic-values*)) :into pre
        :collect `(pop (gethash ',name *dynamic-values*))         :into post
        :finally (return `(unwind-protect (progn ,@pre ,@body) ,@post))))

;;; > (progn
;;;     (f)
;;;     (let-dynamic ((*x* 50))
;;;       (f)
;;;       (setf *x* 2)
;;;       (f))
;;;     (f))
;;; *x* is 1
;;; *x* is 50
;;; *x* is 2
;;; *x* is 1
