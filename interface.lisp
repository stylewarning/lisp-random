;;;; interface.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith

(ql:quickload :alexandria)

;;; A simple data structure to store argument lists and how it should
;;; be used. USE-APPLY essentially says whether ARGS is a spreadable
;;; argument list designator.
(defstruct arglist
  args
  use-apply)

(defun extend-arglist (arglist &rest args)
  "Extend the arglist ARGLIST with the arguments ARGS. Creates a new arglist."
  (make-arglist :args (append (arglist-args arglist) args)
                :use-apply (arglist-use-apply arglist)))

(defun make-arglist-use-apply (arglist)
  "Create an arglist identical to the arglist ARGLIST, except specifying that it must use APPLY."
  (make-arglist :args (arglist-args arglist)
                :use-apply t))

(defun arglist-form (function-form arglist)
  "Construct the calling form with the function form FUNCTION-FORM and the arglist ARGLIST."
  (if (arglist-use-apply arglist)
      `(apply ,function-form ,@(arglist-args arglist))
      `(funcall ,function-form ,@(arglist-args arglist))))

(defun calling-form (function-form lambda-list)
  "Given an ordinary lambda list LAMBDA-LIST and a FUNCALLable FUNCTION-FORM, construct a form which calls FUNCTION-FORM on all of the arguments specified by LAMBDA-LIST."
  (multiple-value-bind 
        (required-args
         optional-args                 ; List of (NAME INIT SUPPLIEDP)
         rest-arg
         kw-args                  ; List of ((KW NAME) INIT SUPPLIEDP)
         allow-other-keys
         aux-args)
        (alexandria:parse-ordinary-lambda-list lambda-list)
    (declare (ignore aux-args))       ; Only used for local variables.
    (labels ((process-lambda-list (arglist)
               (process-optionals arglist optional-args))
             
             ;; Optional argument handling. The general idea is to run
             ;; through one by one, dealing with suppliedness checking
             ;; as needed. Unless we quit out early, continue on to
             ;; &rest-argument handling.
             (process-optionals (arglist optionals)
               (if (null optionals)
                   (process-rest arglist rest-arg)
                   (destructuring-bind (name init suppliedp)
                       (car optionals)
                     (declare (ignore init))
                     (if suppliedp
                         `(if ,suppliedp
                              ;; Here, we know the optional argument
                              ;; has been supplied, so we add it and
                              ;; continue.
                              ,(process-optionals
                                (extend-arglist arglist name)
                                (cdr optionals))
                              ;; Here, we know the optional argument
                              ;; was *not* supplied. This implies that
                              ;; no arguments follow, so we halt
                              ;; processing and return our purchase.
                              ,(arglist-form function-form arglist))

                         ;; No suppliedness checking. Add the argument
                         ;; and continue.
                         (process-optionals
                          (extend-arglist arglist name)
                          (cdr optionals))))))
             
             ;; The &rest argument really bungles everything. It can
             ;; turn a nice FUNCALL into an APPLY. It can also usurp
             ;; the use of keyword arguments.
             ;;
             ;; There is hope though. If there is an &key in the
             ;; presence of an &rest, then the &rest is just going to
             ;; contain the keys. However, we have to be careful,
             ;; because &allow-other-keys can allow for unspecified
             ;; keys and we need to account for that.
             (process-rest (arglist rest-argument)
               (if rest-argument
                   (let ((rest-arglist (extend-arglist
                                        (make-arglist-use-apply arglist)
                                        rest-argument)))
                     ;; We have an &rest argument. Decide on whether
                     ;; we should continue processing keys, or just
                     ;; return and call it a day and bail out if we
                     ;; have a pesky &allow-other-keys.
                     (cond
                       ;; Call it a day and bail out because we have a
                       ;; pesky &allow-other-keys.
                       (allow-other-keys (arglist-form function-form
                                                       rest-arglist))
                       
                       ;; We have keyword arguments, so we go ahead
                       ;; and process those. This is a lucky case
                       ;; because we don't have to have the &rest
                       ;; argument.
                       (kw-args (process-keys arglist kw-args))
                       
                       ;; We have an &rest argument and no keys, so we
                       ;; must account for it and use APPLY with a
                       ;; spreadable argument list designator.
                       (t (process-keys rest-arglist kw-args))))
                   
                   ;; We have no &rest to worry about.
                   (process-keys arglist kw-args)))
             
             ;; The &key arguments are only processed if there is no
             ;; &rest form to be found. These need to be processed in
             ;; much the same way as optionals, except since there is
             ;; no order dependence, suppliedness checking doesn't
             ;; cause anything to short circuit.
             (process-keys (arglist keyword-specs)
               (if (null keyword-specs)
                   ;; We are finally done processing
                   ;; everything. Return the form.
                   (arglist-form function-form arglist)
                   
                   ;; We have keyword arguments left to process. Take
                   ;; one and process it. This is very similar to
                   ;; optionals processing.
                   (destructuring-bind ((key name) init suppliedp)
                       (car keyword-specs)
                     (declare (ignore init))
                     (if suppliedp
                         `(if ,suppliedp
                              ;; Here, we know the keyword argument
                              ;; has been supplied, so we add it and
                              ;; continue.
                              ,(process-keys
                                (extend-arglist arglist key name)
                                (cdr keyword-specs))
                              ;; Here, we know the keyword argument
                              ;; was *not* supplied. Just continue
                              ;; processing.
                              ,(process-keys arglist (cdr keyword-specs)))

                         ;; No suppliedness checking. Add the argument
                         ;; and continue.
                         (process-keys
                          (extend-arglist arglist key name)
                          (cdr keyword-specs)))))))
      
      ;; Go through the chain of lambda list processing. Start with an
      ;; arglist containing all of the required arguments.
      (process-lambda-list (make-arglist :args required-args)))))

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
