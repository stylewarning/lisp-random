;;;; coroutine.lisp
;;;;
;;;; Author: Robert Smith

(ql:quickload :cl-cont)

(defmacro coroutine (&body body)
  "Create a funcallable coroutine whose body is BODY.

Within BODY, the unary function YIELD may be called which will immediately return the argument provided. Upon reinvoking the coroutine, control will start where YIELD left off."
  (let ((entry (gensym "ENTRY-"))
        (exit  (gensym "EXIT-"))
        (continuation (gensym "CONTINUATION-"))
        (return-value (gensym "RETURN-VALUE-"))
        (coroutine (gensym "COROUTINE-"))
        (resume-value (gensym "RESUME-VALUE-")))
    `(cl-cont:with-call/cc
       (let ((,entry nil)
             (,exit nil))
         (labels ((yield (,return-value)
                    (cl-cont:let/cc ,continuation
                      ;; Save the point we need to start at next.
                      (setf ,entry ,continuation)
                      
                      ;; Call the continuation to exit out.
                      (funcall ,exit ,return-value)))
                  
                  (,coroutine (,resume-value)
                    (cl-cont:let/cc ,continuation
                      ;; Remember how to exit when we yield.
                      (setf ,exit ,continuation)
                      
                      ;; Restart at our save point.
                      (funcall ,entry ,resume-value))))
           
           ;; Set our initial entry point into our function.
           ;;
           ;; RESUME-VALUE could be used as a value to resume a
           ;; computation with, essentially rebinding the lexical
           ;; state of your function. Here, it's just ignored.
           (setf ,entry (lambda (,resume-value)
                          (declare (ignorable ,resume-value))
                          (prog1 (progn ,@body)
                            ;; After our body has finished executed.
                            (setf ,entry (lambda (,resume-value)
                                           (declare (ignore ,resume-value))
                                           (cerror "Return NIL."
                                                   "Coroutine exhausted."))))))
           ;; Returned coroutine.
           (lambda () (,coroutine nil)))))))
