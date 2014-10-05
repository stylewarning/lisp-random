;;;; named-arguments.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith

(ql:quickload :alexandria)

;;; Named arguments are a facility I wish Common Lisp had. They are
;;; required arguments which are passed into a function by way of
;;; &KEY-like syntax.
;;;
;;; Ideally there would be another lambda list keyword, &NAMED. We
;;; could do lambda list parsing here, but we will instead just make a
;;; new DEFUN form. This has the disadvantage that we can't mix and
;;; mingle other lambda list features.
;;;
;;; This implementation is semi-optimized. If FUNCALL or APPLY are
;;; used, then checks for named argument existence will be done at
;;; runtime. If the function is in standard function call position,
;;; then the call will be converted into a &KEY-less call (as if all
;;; of the arguments were required lambda list parameters).
;;;
;;; See the end of the file for example interactions.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun keywordify (symbol)
    (values (intern (symbol-name symbol) :keyword)))

  (defun lambda-list-keyword-p (symbol)
    (and (member symbol lambda-list-keywords)
         t)))

(defmacro defun-named (name arguments &body body)
  (check-type name symbol)
  (assert (every #'symbolp arguments) (arguments)
          "Argument names to a function defined with DEFUN-NAMED should ~
           be symbols. Found ~{~S~^, ~}."
          (remove-if #'symbolp arguments))
  (assert (notany #'lambda-list-keyword-p arguments) (arguments)
          "Lambda list keywords in a function defined with DEFUN-NAMED ~
           are prohibited. Found ~{~S~^, ~}."
          (remove-if-not #'lambda-list-keyword-p arguments))
  (labels ((exists-p-name (name)
             (gensym (concatenate 'string
                                  (symbol-name name)
                                  "-EXISTS-P"))))
    (let* ((form (gensym "FORM-"))
           ;; Name of the function that does not check for validity of
           ;; the arguments' existences.
           (name-unchecked (gensym (concatenate 'string
                                                (symbol-name name)
                                                "-")))
           ;; Names of the ARG-EXISTS-P variables.
           (arguments-exists-p (mapcar #'exists-p-name arguments))
           ;; Valid lambda list parameters (ARG NIL ARG-EXISTS-P).
           (arguments-with-existence-checking (mapcar
                                               (lambda (arg arg-exists-p)
                                                 `(,arg nil ,arg-exists-p))
                                               arguments
                                               arguments-exists-p)))
      (multiple-value-bind (plain-body declarations documentation)
          (alexandria:parse-body body :documentation t)
        `(progn
           ;; Define the function which does checks by default.
           (defun ,name (&key ,@arguments-with-existence-checking)
             ,@(and documentation (list documentation))
             ,@declarations
             ,@(loop :for argument          :in arguments
                     :for argument-exists-p :in arguments-exists-p
                     :collect `(assert ,argument-exists-p
                                       (,argument)
                                       "The argument ~S is required but ~
                                        was not provided."
                                       ',argument))
             ,@plain-body)
           
           ;; Define a version of the function that does not do argument
           ;; existence checking. They will be guaranteed to exist.
           ;;
           ;; We do away with &KEY here since we know the call is valid.
           (defun ,name-unchecked (,@arguments)
             ,@body)
           
           ;; Define a compiler macro to ensure calls to the function are
           ;; valid.
           (define-compiler-macro ,name (&whole ,form &key ,@arguments)
             (declare (ignore ,@arguments))
             (let ((arguments ',(mapcar #'keywordify arguments))
                   (found-arguments (make-hash-table))
                   (forms-to-evaluate nil))
               ;; Sanity check the number of arguments. It should be even.
               (unless (evenp (length arguments))
                 (warn "There are an uneven number of arguments ~
                        provided to the function ~S, which implies ~
                        there are probably unmatched named arguments."
                       ',name))
               
               ;; Make known all of the required arguments.
               (dolist (argument arguments)
                 (setf (gethash argument found-arguments) nil))
               
               ;; Make a note of all of the arguments found in the
               ;; calling form. While we iterate through the
               ;; arguments, we will collect their values which will
               ;; be used to ensure left-to-right evaluation.
               (loop :for key-args :on (cdr ,form) :by #'cddr
                     :for key := (car key-args)
                     :for arg := (cadr key-args)
                     :do (setf (gethash key found-arguments) t)
                         (push (cons key arg) forms-to-evaluate))

               (setf forms-to-evaluate (nreverse forms-to-evaluate))
               
               ;; Emit warnings for arguments that weren't found.
               (maphash (lambda (argument existsp)
                          (unless existsp
                            (warn "Missing named argument ~S to the function ~S."
                                  argument
                                  ',name)))
                        found-arguments)
               
               ;; Expand into the unchecked form. We have to evaluate
               ;; the arguments in the order they were provided, then
               ;; provide them in the order the unchecked function
               ;; expects.
               (let ((generated-args (make-hash-table :test 'equal)))
                 ;; Populate the generated args.
                 (dolist (argument arguments)
                   (setf (gethash (symbol-name argument) generated-args)
                         (gensym (symbol-name argument))))
                  
                 (flet ((arg-key-to-generated-arg (key)
                          (gethash (symbol-name key) generated-args))
                        (arg-to-generated-arg (arg)
                          (gethash (symbol-name arg) generated-args)))
                   
                   ;; Generate the LET form which evaluates the
                   ;; arguments in the order they were provided.
                   `(let* ,(loop :for (key . arg) :in forms-to-evaluate
                                 :collect (list (arg-key-to-generated-arg key)
                                                arg))
                      ;; Call the unchecked function with the
                      ;; arguments in the order they were specified in
                      ;; the definition.
                      (,',name-unchecked
                       ,@(mapcar #'arg-to-generated-arg arguments))))))))))))



;;; CL-USER> (defun-named foo (a b)
;;;            (+ a b))
;;; FOO
;;; CL-USER> (compile nil '(lambda () (foo :a 1)))
;;; ; in: LAMBDA ()
;;; ;     (FOO :A 1)
;;; ; 
;;; ; caught WARNING:
;;; ;   Missing named argument :B to the function FOO.
;;; ; 
;;; ; compilation unit finished
;;; ;   caught 1 WARNING condition
;;; #<FUNCTION (LAMBDA ()) {10068D233B}>
;;; T
;;; T
;;; CL-USER> (compile nil '(lambda () (foo :b 2 :a 1)))
;;; #<FUNCTION (LAMBDA ()) {10069A419B}>
;;; NIL
;;; NIL
;;; CL-USER> (compile nil '(lambda () (foo :b 2 :a)))
;;; ; in: LAMBDA ()
;;; ;     (FOO :B 2 :A)
;;; ; 
;;; ; caught WARNING:
;;; ;   Error during compiler-macroexpansion of (FOO :B 2 ...). Use *BREAK-ON-SIGNALS*
;;; ;   to intercept.
;;; ;   
;;; ;    error while parsing arguments to DEFINE-COMPILER-MACRO FOO:
;;; ;      odd number of elements in keyword/value list: (:B 2 :A)
;;; ; 
;;; ; caught STYLE-WARNING:
;;; ;   The function has an odd number of arguments in the keyword portion.
;;; ; 
;;; ; compilation unit finished
;;; ;   caught 1 WARNING condition
;;; ;   caught 1 STYLE-WARNING condition
;;; #<FUNCTION (LAMBDA ()) {1006A1A02B}>
;;; T
;;; T
;;; CL-USER> (funcall #'foo :a 1)
;;; The argument B is required but was not provided.
;;;    [Condition of type SIMPLE-ERROR]
;;; CL-USER> (apply #'foo '(:a 1))
;;; The argument B is required but was not provided.
;;;    [Condition of type SIMPLE-ERROR]
;;; CL-USER> (let ((x 0))
;;;            (values (foo :b (incf x) :a x)
;;;                    x))
;;; 2
;;; 1
;;; CL-USER> (let ((x 0))
;;;            (values (foo :b x :a (incf x))
;;;                    x))
;;; 1
;;; 1
