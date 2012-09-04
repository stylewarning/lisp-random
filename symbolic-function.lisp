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

(defvar +function-arrow+
  #+unicode "↦"
  #-unicode "->"
  "The arrow printed in the printed representation of a
  SYMBOLIC-FUNCTION.")

(defmethod print-object ((sf symbolic-function) stream)
  (print-unreadable-object (sf stream)
    (with-slots (parameters expression) sf
      (format
       stream
       "~S ~A ~S"
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

;;; Note that a SLAMBDA body does not have a &BODY part of its lambda
;;; list to keep in spirit with the value-producing forms.
(defmacro slambda (lambda-list body)
  `(make-instance 'symbolic-function :parameters ',lambda-list
                                     :expression ',body))

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

(defun function-arity (sf)
  "What is the arity of the symbolic function SF?"
  (length (symbolic-function.parameters sf)))

(defun bound-variable-p (var sf)
  "Is the variable VAR bound in the symbolic function SF?"
  (and (member var (symbolic-function.parameters sf) :test 'eq)
       t))

(defun free-variable-p (var sf)
  "Is the variable VAR free in the symbolic function SF?"
  (not (bound-variable-p var sf)))

(defun nullary-function-p (sf)
  "Is the symbolic function SF nullary?"
  (zerop (function-arity sf)))

(defun unary-function-p (sf)
  "Is the symbolic function SF unary?"
  (= 1 (function-arity sf)))

(defun binary-function-p (sf)
  "Is the symbolic function SF binary?"
  (= 2 (function-arity sf)))


;;; XXX: This should probably make a copy/new function.
(defun alpha-convert (sf new-var &optional old-var)
  (unless (nullary-function-p sf)
    (with-slots (parameters expression) sf
      (unless old-var
        (setf old-var (first parameters)))
      
      (if (bound-variable-p new-var sf)
          (error "Cannot alpha convert ~S to ~S because ~S is bound in ~S."
                 old-var
                 new-var
                 new-var
                 sf)
          (let ((subs (list (cons old-var new-var))))
            (nsublis subs parameters)
            (nsublis subs expression)))))
  sf)

;;; XXX: add substitution and beta reduction

;;; Symbolic Manipulation

;;; A very primitive differentiator
(defun diff (expr var)
  (cond
    ((symbolp expr) (if (eq expr var) 1 0))
    ((atom expr)    0)
    (t (case (first expr)
         ((+) `(+ ,(diff (second expr) var)
                  ,(diff (third expr)  var)))
         ((-) `(- ,(diff (second expr) var)
                  ,(diff (third expr)  var)))
         ((*) `(+ (* ,(diff (second expr) var)
                     ,(third expr))
                  (* ,(second expr)
                     ,(diff (third expr) var))))
         ((sin) `(* ,(diff (second expr) var)
                    (cos ,(second expr))))
         ((cos) `(- 0 (* ,(diff (second expr) var)
                         (sin ,(second expr)))))
         (t (error "Don't know how to diff ~S" expr))))))

(defun differentiate (sf)
  (assert (unary-function-p sf)
          (sf)
          "Symbolic function must have exactly one parameter.")
  (let ((var (first (symbolic-function.parameters sf)))
        (expr (symbolic-function.expression sf)))
    (make-symbolic-function (list var)
                            (diff expr var))))