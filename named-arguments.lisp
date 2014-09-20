;;;; named-arguments.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith

;;; Named arguments are a facility I wish Common Lisp had. They are
;;; required arguments which are passed into a function by way of
;;; &KEY-like syntax.
;;;
;;; Ideally there would be another lambda list keyword, &NAMED. We
;;; could do lambda list parsing here, but we will instead just make a
;;; new DEFUN form. This has the disadvantage that we can't mix and
;;; mingle other lambda list features.

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
  (let ((form (gensym "FORM-")))
    `(progn
       ;; Define the function.
       (defun ,name (&key ,@arguments)
         ,@body)
       
       ;; Define a compiler macro to ensure calls to the function are
       ;; valid.
       (define-compiler-macro ,name (&whole ,form &key ,@arguments)
         (declare (ignore ,@arguments))
         (let ((arguments ',(mapcar #'keywordify arguments))
               (found-arguments (make-hash-table)))
           ;; Sanity check the number of arguments. It should be even.
           (unless (evenp (length arguments))
             (warn "There are an uneven number of arguments ~
                  provided to the function ~S, which implies ~
                  there are probably unmatched named arguments."
                   ',name))
           
           ;; Make known all of the required arguments.
           (dolist (argument arguments)
             (setf (gethash argument found-arguments) nil))
           
           ;; Make a note of all of the arguments found in the calling
           ;; form.
           (loop :for key-args :on (cdr ,form) :by #'cddr
                 :for key := (car key-args)
                 :do (setf (gethash key found-arguments) t))
           
           ;; Emit warnings for arguments that weren't found.
           (maphash (lambda (argument existsp)
                      (unless existsp
                        (warn "Missing named argument ~S to the function ~S."
                              argument
                              ',name)))
                    found-arguments)
           
           ;; Decline to expand to anything else.
           ,form)))))



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
