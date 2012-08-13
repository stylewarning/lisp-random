;;;; symbolic-function.lisp
;;;; Copyright (c) 2012 Robert Smith

;;; Requires MOP.
#-closer-mop
(ql:quickload "CLOSER-MOP")

;;; A `symbolic function' is a function that holds both an executable
;;; form of itself and a symbolic form of itself.

(defclass symbolic-function ()
  ((compiled-function :initarg :compiled-function
                      :initform nil
                      :accessor symbolic-function.compiled-function)

   (parameters :initarg :parameters
               :initform nil
               :accessor symbolic-function.parameters)

   (expression :initarg :expression
               :initform nil
               :accessor symbolic-function.expression))
  (:metaclass c2mop:funcallable-standard-class))

(defconstant +function-arrow+ #\↦
  "The arrow printed in the printed representation of a
  SYMBOLIC-FUNCTION.")

(defmethod print-object ((sf symbolic-function) stream)
  (print-unreadable-object (sf stream)
    (with-slots (parameters expression) sf
      (format
       stream
       "~S ~C ~S"
       (if (endp (rest parameters))
           (first parameters)
           parameters)
       +function-arrow+
       expression))))

(defun symbolic-function-lambda-form (sf)
  "Create the lambda form (data) of SF."
  `(lambda ,(symbolic-function.parameters sf)
     ,(symbolic-function.expression sf)))

(defun compile-symbolic-function (sf)
  "(Re)compile a symbolic function."
  (setf (symbolic-function.compiled-function sf)
        (compile nil (symbolic-function-lambda-form sf))))

(defun make-symbolic-function (parameters expression)
  "Make a SYMBOLIC-FUNCTION with parameters PARAMETERS and expression
  EXPRESSION."
  (make-instance 'symbolic-function :parameters parameters
                                    :expression expression))

(defmethod initialize-instance :after ((sf symbolic-function) &key)
  (with-slots (compiled-function parameters expression) sf
    ;; Assert PARAMETERS is a list of symbols.
    
    ;; Grab the symbolic function or compute it, and set the
    ;; FUNCALLABLE-INSTANCE-FUNCTION.
    (let ((lam (or (symbolic-function.compiled-function sf)
                   (compile-symbolic-function sf))))
      (c2mop:set-funcallable-instance-function sf lam))))

(defmacro define-symbolic-function (name param-list expression)
  "Define a symbolic function NAME with the parameters PARAM-LIST,
bound to the expression EXPRESSION."
  `(progn
     (when (fboundp ',name)
       (warn "Redefining (symbolic) function ~S." ',name))
     (setf (symbol-function ',name)
           (make-symbolic-function ',param-list
                                   ',expression))
     ',name))

(defun symbolic-compose (f sf)
  "Compose a regular function F and the symbolic function SF."
  (with-slots (parameters expression) sf
    (make-symbolic-function parameters
                            (list f expression))))
