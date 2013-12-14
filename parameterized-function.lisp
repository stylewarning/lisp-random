;;;; parameterized-function.lisp
;;;; Copyright (c) 2013 Robert Smith

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun constant-quoted-list-p (l &optional env)
    (and (listp l)
         (typep l '(cons (member quote)))
         (every (lambda (x) (constantp x env)) (cadr l))))
  
  (defun dispatch-table-name (name)
    (intern (format nil "%~A-DISPATCH-TABLE" (symbol-name name))))
  
  (defun generate-name-from-parameters (fn params &optional suffix)
    (intern (format nil "%~A-~{~A~^-~}~@[-~A~]"
                    (symbol-name fn)
                    (mapcar #'symbol-name params)
                    suffix)))
  
  (defun parameter-congruent-p (expected-param given-param)
    (or (string= "_" (symbol-name given-param))
        (eql expected-param given-param)))
  
  (defun parameters-congruent-p (expected-params given-params)
    (and (= (length expected-params) (length given-params))
         (every #'parameter-congruent-p expected-params given-params)))
  
  (defun undispatch-form (form new-function-name)
    (cons new-function-name (cddr form))))


(defmacro define-dispatch-function (name (&rest parameters) (&rest args))
  (declare (ignore parameters))
  (let ((table-name (dispatch-table-name name)))
    `(progn
       (defvar ,table-name (make-hash-table :test 'equal))
       
       (define-compiler-macro ,name (&whole form params ,@args &environment env)
         (declare (ignore ,@args))
         (if (not (constant-quoted-list-p params env))
             (progn
               (warn "Non-constant parameters ~S in ~S" params form)
               form)
             (let* ((params (cadr params))    ; remove the QUOTE
                    (dispatch-function (gethash params ,table-name)))
               (if (null dispatch-function)
                   (progn
                     (warn "Unknown dispatch function for ~S with parameters ~S" ',name params)
                     form)
                   (undispatch-form form dispatch-function)))))
       
       (defun ,name (params &rest args)
         (let ((dispatch-function (gethash params ,table-name)))
           (unless dispatch-function
             (error "Parameters ~S are unknown to the dispatch function ~S" params ',name))
           (warn "Dynamic dispatch occuring in ~S" ',name)
           (apply dispatch-function args))))))

(defmacro define-parameterized-function (name (&rest parameters) (&rest args) &body body)
  ;; check PARAMETERS type
  (let ((dispatch-table    (dispatch-table-name name))
        (dispatch-function (generate-name-from-parameters name parameters)))
    `(progn
       (declaim (inline ,dispatch-function))
       (defun ,dispatch-function (,@args)
         ,@body)
       
       (setf (gethash ',parameters ,dispatch-table) ',dispatch-function)
       
       ',name)))


#+#:example
(progn
  (define-dispatch-function mult (xtype ytype) (x y))

  (define-parameterized-function mult (:integer :integer) (x y)
    (* x y))

  (define-parameterized-function mult (:integer :string) (x y)
    (with-output-to-string (s)
      (loop :repeat x
            :do (write-string y s))))

  (define-parameterized-function mult (:string :integer) (y x)
    (with-output-to-string (s)
      (loop :repeat x
            :do (write-string y s))))

  (define-parameterized-function mult (:string :string) (x y)
    (concatenate 'string x y)))
