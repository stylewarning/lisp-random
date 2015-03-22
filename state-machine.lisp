;;;; state-machine.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith
;;;;
;;;; This file is an attempt to build a relatively efficient state
;;;; machine abstraction.

;;;; Bugs: SBCL complains about modifying constant data. I don't know if it is right or if I am right.

;;; (ql:quickload :alexandria)

(defclass state-machine ()
  ((states :initarg :states
           :reader state-machine-states
           :documentation "A list of the names of each state.")
   (transition-graph :initarg :transition-graph
                     :reader state-machine-transition-graph
                     :documentation "A hash table containing the state names as keys and a list of possible transitions as values.")
   (invocation-function :initarg :invocation-function
                        :reader state-machine-invocation-function
                        :documentation "The multivariate function to invoke to enter the state machine.")))

(defgeneric invoke-state-machine (sm &rest args)
  (:method ((sm state-machine) &rest args)
    (apply (state-machine-invocation-function sm) args)))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun contains-duplicates-p (list &key (test 'eql))
  (/= (list-length list)
      (list-length (remove-duplicates list :test test))))
)

(defmacro transition (state &rest args)
  (declare (ignore state args))
  (error "Transition is an internal macro to DEFINE-STATE-MACHINE."))

(defmacro state-machine (args state-definitions &body body)
  "Create a state machine which takes the arguments ARGS the body BODY. Return a state machine object which can be invoked with the arguments ARGS.

STATE-DEFINITIONS is a list of state definitions. It looks much like the definition of a LABELS function definition:

    (state-name arguments
      [documentation]
      [declarations]
      forms ...)

Within the state definitions, however, there is a macro defined called TRANSITION which transitions to one of the other states immediately without return. (The use of TRANSITION ensures that a tail call will happen.)

BODY can enter the state machine 

\(It is allowed, but discouraged, to bypass the use of TRANSITION by simply calling the state name as a function instead.)"
  (let ((states (mapcar #'first state-definitions))
        (transition-graph (make-hash-table)))
    (labels ((construct-state (state-def)
               (destructuring-bind (name args . body) state-def
                 (multiple-value-bind (forms decls doc)
                     (alexandria:parse-body body :documentation t)
                   ;; This is a LABELS function definition.
                   `(,name ,args
                      ,@(and doc (list doc))
                      ,@decls
                      (macrolet ((transition (state &rest args)
                                   (unless (member state ',states)
                                     (error "The state ~S in the transition ~
                                             occurring in the state ~S ~
                                             is not a valid."
                                            state
                                            ',name))
                                   (pushnew state
                                            (gethash ',name ,transition-graph))
                                   `(return-from ,',name
                                      (,state ,@args))))
                        ,@forms))))))
      (when (contains-duplicates-p states)
        (warn "There are duplicate state names in the state machine."))
      (multiple-value-bind (forms decls doc)
          (alexandria:parse-body body :documentation t)
        `(make-instance
          'state-machine
          :states ',(remove-duplicates states)
          :transition-graph ',transition-graph
          :invocation-function
          (lambda ,args
            ,@(and doc (list doc))
            ,@decls
            (labels ,(mapcar #'construct-state state-definitions)
              (macrolet ((transition (state &rest args)
                           (unless (member state ',states)
                             (error "The state ~S in the initial transition ~
                                     is not a valid."
                                    state))
                           (pushnew state
                                    (gethash nil ,transition-graph))
                           `(,state ,@args)))
                ,@forms))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Examples ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *beer-machine*
  (state-machine (count)
      ((decide (current-count)
         (if (= 1 current-count)
             (transition last-case)
             (transition general-case current-count)))
       (general-case (current-count)
         (format t "~D bottles of beer on the wall, ~D bottles of beer.~%~
                    Take one down, pass it around, ~D bottle~:P of beer on ~
                    the wall...~%"
                 current-count
                 current-count
                 (1- current-count))
         (transition decide (1- current-count)))
       (last-case ()
         (format t "If that one bottle should happen to fall, what a waste ~
                    of alcohol!")))
    (transition decide count)))

(defparameter *tail-call-test*
  (state-machine (x)
      ((even (x)
             (if (zerop x)
                 t
                 (transition odd (1- x))))
       (odd (x)
            (if (zerop x)
                nil
                (transition even (1- x)))))
    (even x)))
