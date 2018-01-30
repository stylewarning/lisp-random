;;;; delay.lisp
;;;; Copyright (c) 2012-2018 Robert Smith

(defpackage #:lazy
  (:use #:cl)
  (:shadow ;; #:cons
           #:car
           #:cdr
           #:append)
  (:export #:promise
           #:force
           #:delay
           #:call-with-delayed-arguments
           #:lazycall
           #:deflazy
           #:enable-lazy-syntax
           ;; Shadowed symbols
           ;; #:cons
           #:car
           #:cdr
           #:append
           #:integers
           #:repeat
           #:take
           #:force-tree
           ))

(in-package #:lazy)

;;;; A small library for lazy function evaluation, including optional
;;;; special syntax.

(defun print-promise (object stream depth)
  (declare (ignore depth))
  (print-unreadable-object (object stream :type t :identity t)))

(defstruct (promise (:conc-name promise.)
                    (:predicate promisep)
                    (:print-function print-promise))
  (forcedp nil :type boolean)
  (value   nil :type t))

(defun force (promise)
  "Force a promise PROMISE. Return the value of the promise and
memoize it. If something that's not a PROMISE is given, return it as-is."
  (typecase promise
    (promise
     (if (promise.forcedp promise)
         (promise.value promise)
         (setf (promise.forcedp promise) t
               (promise.value promise) (force (funcall (promise.value promise))))))
    (t promise)))

(defmacro delay (&body computation)
  "Delay the computation of COMPUTATION, creating a promise."
  `(make-promise :value (lambda () ,@computation)))

(defmacro call-with-delayed-arguments (func &rest arguments)
  "Call FUNC with the arguments ARGUMENTS delayed."
  `(funcall ,func
            ,@(loop :for argument :in arguments
                    :collect `(delay ,argument))))

;;; Synonym for CALL-WITH-DELAYED-ARGUMENTS
(defmacro lazycall (func &rest arguments)
  `(call-with-delayed-arguments ,func ,@arguments))

(defmacro deflazy (name (&rest args) &body body)
  (let ((genargs (mapcar (lambda (x)
                           (declare (ignore x))
                           (gensym))
                         args)))
    `(defun ,name ,args
       (let ,(mapcar #'list genargs args)
         (declare (ignorable ,@genargs))
         (symbol-macrolet ,(mapcar (lambda (arg genarg)
                                     `(,arg (force ,genarg)))
                            args
                            genargs)
           ,@body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; NOTATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun enable-lazy-syntax ()
  (set-macro-character
   #\[
   (lambda (stream char)
     (declare (ignore char))
     (let* ((lst (read-delimited-list #\] stream t)))
       (if (null lst)
           nil
           (destructuring-bind (f . args) lst
             `(lazycall ,(if (listp f)
                             f
                             `(function ,f))
                        ,@args))))))

  (set-macro-character
   #\]
   (get-macro-character #\))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EXAMPLES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(enable-lazy-syntax)

(deflazy car (c)
  (assert (consp c))
  (force (cl:car c)))

(deflazy cdr (c)
  (assert (consp c))
  (force (cl:cdr c)))

(defun append (x y)
  (delay
    (if (null (force x))
        y
        [cons (car x) (append (cdr x) y)])))

(defun integers (&optional (start 0))
  (delay (cons start (integers (1+ start)))))

(defun repeat (x)
  (delay (cons x (repeat x))))

(deflazy take (n x)
  (cond
    ((zerop n) nil)
    ((null x)  nil)
    (t (delay [cons (car x) (take (1- n) (cdr x)) ]))))

(deflazy force-tree (x)
  (cond
    ((atom x) x)
    ((consp x) (cons (force-tree (car x)) (force-tree (cdr x))))))

(deflazy if* (pred then else)
  (if pred then else))

(defun test-if* ()
  (if (functionp #'if*)
      (format t "Yes ~S is a function!" 'if*)
      (format t "ERROR! ~S is not a function!" 'if*))

  (terpri)

  (lazycall 'if*
            t
            (format t "~S worked!~%" 'if*)
            (error "This shouldn't get called.")))
