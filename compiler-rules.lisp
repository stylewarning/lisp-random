;;;; compiler-rules.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

(defvar *compiler-rules* (make-hash-table :test 'eq))

(defun compiler-rule (name rule-name)  
  (let ((rules (gethash name *compiler-rules*)))
    (when rules
      (gethash rule-name rules))))

(defun (setf compiler-rule) (new-value name rule-name)
  (check-type new-value (or null function symbol))
  (check-type name symbol)
  (check-type rule-name symbol)
  (let ((rules (gethash name *compiler-rules*)))
    (unless rules
      ;; Create a new rules table.
      (setf rules (make-hash-table :test 'eq)
            (gethash name *compiler-rules*) rules))
    (if (null new-value)
        (remhash rule-name rules)
        (setf (gethash rule-name rules) new-value))))

;;; Local macros to DEFINE-COMPILER-RULE.

(defmacro give-up ()
  (error "GIVE-UP is a local macro to DEFINE-COMPILER-RULE."))

(defmacro environment ()
  (error "ENVIRONMENT is a local macro to DEFINE-COMPILER-RULE."))

;;; DEFINE-COMPILER-RULE implementation

(defmacro define-compiler-rule (name rule-name arglist &body body)
  (let ((whole (gensym "WHOLE-"))
        (block-name (gensym (format nil "~A ~A-" name rule-name)))
        (environment (gensym "ENVIRONMENT-")))
    `(progn
       ;; Create a compiler macro if one hasn't been created.
       ,(when (null (compiler-macro-function name))
          `(define-compiler-macro ,name
               (&whole ,whole ,@arglist &environment ,environment)
             (let ((rules (gethash ',name *compiler-rules* (make-hash-table))))
               (loop :named rule-loop
                     :for rule-name :being :the :hash-keys :of rules
                       :using (hash-value rule-function) :do
                         (format t "~&Firing rule ~A~%" rule-name)
                         (let ((result (funcall rule-function ,whole ,environment)))
                           (unless (eql ,whole result)
                             (format t "Rule succeeded!~%")
                             (return-from rule-loop result)))
                         (format t "Rule failed!~%")
                     :finally (return-from rule-loop ,whole)))))
       
       ;; Add the new compiler rule.
       (setf (compiler-rule ',name ',rule-name)
             (lambda (,whole &optional ,environment)
               (block ,block-name
                 (macrolet ((give-up ()
                              `(return-from ,',block-name ,',whole))
                            (environment ()
                              ,environment))
                   (destructuring-bind ,arglist (cdr ,whole)
                     ,@body))))))))

;;;; test

(defun multiply (x y)
  (* x y))

(define-compiler-rule multiply by-1 (x y)
  (cond
    ((eql x 1) y)
    ((eql y 1) x)
    (t (give-up))))

(define-compiler-rule multiply by-2 (x y)
  (cond
    ((eql x 2) `(+ ,y ,y))
    ((eql y 2) `(+ ,x ,x))
    (t (give-up))))
