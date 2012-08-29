;;;; macroexpand-count.lisp
;;;; Copyright (c) 2012 Robert Smith

;;; Count the number of macroexpansions taken to execute a form.

;;; Note that this is not guaranteed to terminate: the special form
;;; SETQ is not guaranteed to not be a macro per section
;;; 3.1.2.1.2.2. If it was a macro, then *MACROEXPAND-HOOK* would get
;;; called infinitely recursively.
;;; 
;;; In order to make this re-usable, we don't use a global
;;; variable. Even if we did use a global variable, and reset its
;;; value at the end, we couldn't nest COUNT-MACRO-EXPANSIONS, so we'd
;;; need to use a stack.
;;; 
;;; We can't (COMPILE NIL <lambda>) as the hook because that would
;;; require the ability to compile a closure with COMPILE.
;;; 
;;; Another option is to pollute the namespace with another function
;;; and force COMPILE to be called on it.
;;; 
;;; We don't choose any of these options and assume that compilers
;;; won't implement macros for SETQ.
(defmacro count-macro-expansions (&body body)
  (let ((count (gensym "COUNT-"))
        (hook  (gensym "HOOK-")))
    `(let ((,count 0))
       (flet ((,hook (expander form env)
                (setq ,count (+ 1 ,count))
                (funcall expander form env)))
         (let ((*macroexpand-hook* #',hook))
           (values (progn ,@body)
                   ,count))))))