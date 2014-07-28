;;;; interface.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith

;;;; This file contains an implementation of interfaces and
;;;; implementations. They're sometimes called protocols in other
;;;; languages.
;;;;
;;;; Broadly speaking, an "interface" is some collection of function
;;;; "prototypes" that a valid implementation must implement. For
;;;; example, something called a "stack" must implement stack
;;;; creation, pushing, peeking, and popping.
;;;;
;;;; The notion of interfaces and implementations aid the distinction
;;;; between data structures and different implementations of that
;;;; data structure. This was perhaps pioneered by Modula-3, and
;;;; became a significant part of other languages like Standard ML and
;;;; OCaml. In all of the aforementioned languages, interfaces can
;;;; actually contain more than just functions, such as types and
;;;; values. Haskell typeclasses are also a form of interface and
;;;; implementation. They are very general and are even parametric.
;;;;
;;;; One way to accomplish the notion of interfaces and
;;;; implementations in Lisp is to use some "abstract class" and make
;;;; several (final) subclasses of that class. The interface, in this
;;;; case, is the abstract class and a collection of generic
;;;; functions. The implementation would be the final subclass along
;;;; with method definitions.
;;;;
;;;; For example:
;;;;
;;;;     (defclass stack () ())
;;;;     (defgeneric make-stack (impl))
;;;;     (defgeneric stack-push (impl s x))
;;;;     (defgeneric stack-pop (impl s))
;;;;     (defgeneric stack-peek (impl s))
;;;;
;;;;     (defclass list-stack (stack) ())
;;;;     (defmethod make-stack ((impl list-stack))
;;;;       nil)
;;;;     (defmethod stack-push ((impl list-stack) s x)
;;;;       (cons x s))
;;;;     (defmethod stack-pop ((impl list-stack) s)
;;;;       (cdr s))
;;;;     (defmethod stack-peek ((impl list-stack) s)
;;;;       (car s))
;;;;
;;;; This is mostly sufficient, though Lisp makes no guarantee that a
;;;; class will have any set of methods defined for it. (One could
;;;; perhaps use the MOP for this.) One can "optimize" implementations
;;;; by conflating the notion of an implementation with the actual
;;;; data structure being implemented, and make it a part of the
;;;; implementation class. In this case, we could have a slot in
;;;; LIST-STACK holding the list.
;;;;
;;;; Since methods are not tied to classes, this implementation allows
;;;; one to have a class implement several methods. Also, it is
;;;; entirely possible to do away with the superclass; that is a
;;;; formality tying all implementations to a particular interface
;;;; with a name.
;;;;
;;;; As I understand, this basic notion is taken to the extreme with
;;;; Fare's Lisp Interface Library
;;;; (http://www.cliki.net/lisp-interface-library).
;;;;
;;;;
;;;; In this file, however, we take a different approach
;;;; entirely. Instead of using a class to represent interfaces and
;;;; implementations, we have a structure whose slots are the
;;;; implementation functions. The name of the structure (which
;;;; decides what slots it has) is the interface, and the
;;;; implementation is the actual slot values.
;;;;
;;;; It is cumbersome, however, to use an interface by accessing slots
;;;; all of the time. Instead, we define functions---which correspond
;;;; to the slot names---which access the slots of an implementation
;;;; and pass the arguments to it.
;;;;
;;;; In doing this, there's no dispatch on type required, just access
;;;; on the slots of the structure. It also forces data structures and
;;;; the interface to be completely disjoint entities.
;;;;
;;;; See the end of the file for an example.
;;;;
;;;;
;;;; There's still some work to do, namely optimizing the
;;;; calls. There's also work in producing better error
;;;; messages. Lambda list congruence would be a start. Showing all
;;;; unimplemented functions in an error message would also be nice.
;;;;
;;;; I am also not convinced that the DEFINE-IMPLEMENTATION syntax is
;;;; as good as it can be.

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
                         (symbol-name slot))))
  
  (defun interface-implementation-constructor-name (intf-name)
    (intern (format nil "MAKE-~A-IMPLEMENTATION" intf-name))))

(defmacro define-interface (name args &body specs)
  (check-type name symbol)
  (assert (null args))
  (let ((intf (gensym "IMPL-"))
        (conc-name (interface-conc-name name)))
    `(progn
       (defstruct (,name (:conc-name ,(intern conc-name))
                         (:constructor ,(interface-implementation-constructor-name name))
                         (:print-function (lambda (obj stream depth)
                                            (declare (ignore depth))
                                            (print-unreadable-object (obj stream :type t :identity t)
                                              (write-string "Implementation" stream))))
                         (:copier nil)
                         (:predicate nil))
         ,@(loop :for spec :in specs
                 :collect `(,(first spec)
                            (error  ,(format nil "Required implementation for ~A in the ~A interface." (first spec) name))
                            :read-only t
                            :type function)))
       
       ;; function definitions
       ,@(loop :for spec :in specs
               :append
               (destructuring-bind (fn-name (&rest lambda-list) &rest rest)
                   spec
                 (declare (ignore rest))
                 `(;progn
                   (declaim (inline ,fn-name))
                   (defun ,fn-name (,intf ,@lambda-list)
                     ;;(declare (dynamic-extent ,intf))
                     ,(calling-form `(the function
                                          (,(interface-accessor name fn-name)
                                           ,intf))
                                    lambda-list)))))
       ',name)))

(defmacro define-implementation (name (intf-name) &body implementations)
  ;; We could do a lot more error checking on the IMPLEMENTATIONS
  ;; list. In particular, we could check that all of the required
  ;; protocol functions are implemented. (This is actually
  ;; implemented, but it happens when trying to call the
  ;; implementation constructor.) We could also ensure that every one
  ;; of them has conforming lambda lists.
  `(defparameter
    ,name
    (,(interface-implementation-constructor-name intf-name)
     ,@implementations)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Example ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+#:ignore
(define-interface stack ()
  (make-stack (&rest r))
  (push-stack (s x))
  (peek-stack (s))
  (pop-stack (s)))

#+#:ignore
(define-implementation list-stack (stack)
  :make-stack
  (lambda (&rest r)
    r)

  :push-stack
  (lambda (s x)
    (cons x s))

  :peek-stack
  (lambda (s)
    (car s))
  
  :pop-stack
  (lambda (s)
    (cdr s)))

#+#:ignore
(define-implementation vector-stack (stack)
  :make-stack
  (lambda (&rest r)
    (let ((length (length r)))
      (make-array length
                  :adjustable t
                  :fill-pointer length
                  :initial-contents r)))
  
  :push-stack
  (lambda (s x)
    (vector-push-extend x s)
    s)
  
  :peek-stack
  (lambda (s)
    (aref s (1- (length s))))
  
  :pop-stack
  (lambda (s)
    (vector-pop s)
    s))

;;; CL-USER> (pop-stack vector-stack
;;;                     (push-stack vector-stack
;;;                                 (make-stack vector-stack 1 2 3)
;;;                                 5))
;;; #(1 2 3)
;;; CL-USER> (pop-stack list-stack
;;;                     (push-stack list-stack
;;;                                 (make-stack list-stack 1 2 3)
;;;                                 5))
;;; (1 2 3)
