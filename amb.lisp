(define-condition amb-fail () ()
  (:report "No amb cases left to try."))

(defun fail ()
  (error 'amb-fail))

(defun amb-call (f thunks)
  (dolist (thunk thunks (fail))
    (handler-case (return (funcall f (funcall thunk)))
      (amb-fail ()))))

(defmacro amb ((var &rest forms) &body body)
  `(amb-call (lambda (,var) ,@body)
             ,(loop :for form :in forms
                    :collect `(lambda () ,form) :into thunks
                    :finally (return (cons 'cl:list thunks)))))
