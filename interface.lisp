;;;; interface.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun interface-conc-name (intf-name)
    (concatenate 'string
                 "%"
                 (symbol-name intf-name)
                 "-"))
  
  (defun interface-accessor (intf-name slot)
    (intern (concatenate 'string
                         (interface-conc-name intf-name)
                         (symbol-name slot)))))

(defmacro define-interface (name args &body specs)
  (check-type name symbol)
  (assert (null args))
  (let ((intf (gensym "INTF-"))
        (conc-name (interface-conc-name name)))
    `(progn
       (defstruct (,name (:conc-name ,(intern conc-name))
                         (:constructor ,(intern (format nil "MAKE-~A-IMPLEMENTATION" name)))
                         (:print-function (lambda (obj stream depth)
                                            (declare (ignore depth))
                                            (print-unreadable-object (obj stream :type t :identity t)
                                              (format stream "~S" "Interface"))))
                         (:copier nil)
                         (:predicate nil))
         ,@(loop :for spec :in specs
                 :collect `(,(first spec)
                            (error  ,(format nil "Required implementation for ~A in the ~A interface." (first spec) name)))))
       
       ;; function definitions
       ,@(loop :for spec :in specs
               :append
               (destructuring-bind (fn-name &optional args &rest rest)
                   spec
                 (declare (ignore rest))
                 `(;progn
                   (declaim (inline ,fn-name))
                   (defun ,fn-name (,intf ,@args)
                     (funcall (,(interface-accessor name fn-name) ,intf)
                              ,@args)))))
       ',name)))

;; Example
(define-interface stack ()
  (make-stack ())
  (push-stack (s x))
  (pop-stack (s)))
