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
memoize it."
  (declare (type promise promise))
  (if (promise.forcedp promise)
      (promise.value promise)
      (setf (promise.forcedp promise) t
            (promise.value promise) (funcall (promise.value promise)))))

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