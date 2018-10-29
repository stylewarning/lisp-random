;;;; Hindley-Milner type inference
;;;; (c) 2010-2012, 2018 Robert Smith

;;;; This is an implementation of Robin Milner's "Algorithm J" from
;;;; his 1978 paper `A Theory of Type Polymorphism in Programming'
;;;; from the Journal of Computer and System Sciences, Volume 17,
;;;; pages 348--375. It is available (as of December 17, 2010) from
;;;; <http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.67.5276v>
;;;; for free.

;;;; This implementation is geared toward a Scheme-like language. See
;;;; the file `tests.lisp' for some examples.

;;;; Also, this needs some cleaning up.

(ql:quickload :optima)

(declaim (optimize (speed 0) safety debug))

;;; Environment routines.
;;; An environment is just an association list.

(defun env-empty ()
  "Return an empty environment."
  nil)

(defun env-update (var val env)
  "Add a binding (VAR . VAL) to ENV."
  (acons var val env))

(defun env-join (env1 env2)
  "Concatenate ENV1 and ENV2."
  #+ignore
  (assert (null (intersection (mapcar #'car env1)
                              (mapcar #'car env2))))
  (append env1 env2))

(defun env-bound-p (var env)
  "Check if VAR exists in ENV."
  (not (null (assoc var env))))

(defun env-value (var env)
  "Get the value of VAR in ENV. This function assumes it exists."
  (cdr (assoc var env)))


;;; Some variable routines for generating unique variables. These are
;;; used to construct type variables in the inference.

(defun make-variable-counter ()
  "Create a new variable counter."
  (let ((variable-count 0))
    (lambda ()
      (incf variable-count)
      (intern (format nil "T~A" variable-count)))))

(defun variablep (x)
  "Check if X is a type variable."
  (and (symbolp x)
       (char= #\T (aref (symbol-name x) 0))))

(defun variable< (x y)
  "Check if variable X was generated before Y."
  (check-type x symbol)
  (check-type y symbol)
  (string< (symbol-name x) (symbol-name y)))


;;; Variable-environment interaction.

(defun variable-val (var env)
  "Get the value of VAR in ENV."
  ;; This recursively substitutes in the event there are multiple
  ;; levels of indirection.
  (if (and (variablep var) (env-bound-p var env))
      (variable-val (env-value var env) env)
      var))

;;; Type Grammar
;;;
;;; Type := (-> Type Type)
;;;       | (* Type Type)
;;;       | (LIST Type)
;;;       | Var
;;;       | BOOL
;;;       | NUM
;;;       | CHAR
;;;       | NIL (unit)

(defun unify (x y &optional (type-env (env-empty)))
  "Unify the types X and Y in the context of the type environment TYPE-ENV. Return a map of unified type variables."
  (let ((xv (variable-val x type-env))
        (yv (variable-val y type-env)))
    (optima:ematch (list xv yv)
      ((list 'NUM 'NUM)   type-env)
      ((list 'BOOL 'BOOL) type-env)
      ((list 'CHAR 'CHAR) type-env)
      ((list '() '())     type-env)
      ;; Always bind later vars to earlier vars.
      ((list (satisfies variablep) (satisfies variablep))
       (cond
         ((variable< xv yv) (env-update yv xv type-env))
         ((variable< yv xv) (env-update xv yv type-env))))
      ((list (satisfies variablep) _) (env-update xv yv type-env))
      ((list _ (satisfies variablep)) (env-update yv xv type-env))
      ((list (list '-> xa xb)
             (list '-> ya yb))
       (unify xb yb (unify xa ya type-env)))
      ((list (list '* xa xb)
             (list '* ya yb))
       (unify xb yb (unify xa ya type-env)))
      ((list (list 'LIST xa)
             (list 'LIST ya))
       (unify xa ya type-env)))
    #+ignore
    (cond
      ((equalp xv yv)
       type-env)
      ((and (variablep xv) (or (not (variablep yv)) (variable< yv xv)))
       (env-update xv yv type-env))
      ((variablep yv)
       (env-update yv xv type-env))
      ((and (consp xv) (consp yv))
       (unify (cdr xv) (cdr yv) (unify (car xv) (car yv) type-env)))
      (t
       (error "Cannot unify structures.")))))

(defun search-tree (x tree &key (test 'eql))
  (cond
    ((consp tree) (or (search-tree x (car tree))
                      (search-tree x (cdr tree))))
    (t (funcall test x tree))))

(defstruct (prefix (:constructor prefix (kind type)))
  (kind nil :type (member :let :letrec :lambda))
  type)

(defun genericp (var typing)
  "Is the type variable VAR a generic variable in TYPING?"
  (if (endp typing)
      t
      (let ((p (cdr (first typing))))
        (if (and (or (eql ':lambda (prefix-kind p))
                     (eql ':letrec (prefix-kind p)))
                 (search-tree var (prefix-type p)))
            nil
            (genericp var (rest typing))))))

(defun instance (x typing counter)
  "Generate an instance of the type expression X with fresh variables
in place of the generic variables defined in TYPING."
  (labels ((instance-aux (x type-env cont)
             ;; The general strategy here is to a maintain table
             ;; mapping the generic type variables to their
             ;; instantiated version.
             (cond
               ((and (variablep x)
                     (genericp x typing))
                (if (env-bound-p x type-env)
                    (funcall cont (env-value x type-env) type-env)
                    (let ((tyvar (funcall counter)))
                      (funcall cont tyvar (env-update x tyvar type-env)))))
               ((consp x)
                (instance-aux (car x) type-env
                              (lambda (a env)
                                (instance-aux (cdr x) env
                                              (lambda (b env)
                                                (funcall cont (cons a b) env))))))
               (t
                (funcall cont x type-env)))))
    (instance-aux x
                  (env-empty)
                  (lambda (a env)
                    (declare (ignore env))
                    a))))


;;; Inference.

(defparameter *global-var-types* (make-hash-table :test 'eql))

(defmacro declare-type (var type &rest var-type-pairs)
  "Declare the type of a variable."
  `(progn
     (setf (gethash ',var *global-var-types*) ',type)
     ,(when var-type-pairs
        `(declare-type ,(car var-type-pairs)
                       ,(cadr var-type-pairs)
                       ,@(cddr var-type-pairs)))))

(defun find-type (prim)
  "Find the type of PRIM. This will not compute the type, it will look
to see if he type of the symbol PRIM is registered."
  (multiple-value-bind (type found?)
      (gethash prim *global-var-types*)
    (if found?
        type
        (error "Don't know about ~A" prim))))

(declare-type +         (-> (* num num) num)
              -         (-> (* num num) num)
              *         (-> (* num num) num)
              /         (-> (* num num) num)
              <         (-> (* num num) bool)
              <=        (-> (* num num) bool)
              =         (-> (* num num) bool)
              >=        (-> (* num num) bool)
              char>?    (-> (* char char) bool)
              char<?    (-> (* char char) bool)
              char<=?   (-> (* char char) bool)
              char=?    (-> (* char char) bool)
              char>=?   (-> (* char char) bool)
              char>?    (-> (* char char) bool)
              cons      (-> (* Ta (list Ta)) (list Ta))
              car       (-> (list Ta) Ta)
              cdr       (-> (list Ta) (list Ta))
              set-car!  (-> (* (list Ta) Ta) ())
              set-cdr!  (-> (* (list Ta) (list Ta)) ())
              null?     (-> (list Ta) bool)
              length    (-> (list Ta) num)
              append    (-> (* (list Ta) (list Ta)) (list Ta))
              reverse   (-> (list Ta) (list Ta))
              map       (-> (* (-> Ta Tb) (list Ta)) (list Tb))
              not       (-> bool bool)
              call/cc   (-> (-> (-> Ta Tb) Tc) Ta)
              apply     (-> (* (-> Ta Tb) (list Ta)) Tb)
              display   (-> Ta ())
              write     (-> Ta ())
              true      bool
              false     bool)

(defun constant-type (x)
  "Determine the type of a constant X with a counter to produce fresh type variables CTR."
  (cond
    ((numberp x)            'num)
    ((characterp x)         'char)
    ((or (eql x 'true)
         (eql x 'false))    'bool)
    ((null x)               '(list Ta))
    ((consp x)
     (let ((element-type (constant-type (car x))))
       (mapc (lambda (y)
               ;; XXX is this EQUALP ok for function types?
               (unless (equalp element-type (constant-type y))
                 (error "List is not homogeneous.")))
             (cdr x))
       `(list ,element-type)))
    (t (error "Unknown constant type."))))

(defun substitute-type-variables (x env)
  "Return X but with each variable in X substituted for the values
defined in ENV."
  (cond ((variablep x) (let ((y (variable-val x env)))
                         (if (variablep y)
                             y
                             (substitute-type-variables y env))))
        ((consp x) (cons (substitute-type-variables (car x) env)
                         (substitute-type-variables (cdr x) env)))
        (t x)))

(defun derive-type (f)
  "Derive the type of expression F and return it using Milner's Algorithm J."
  (let ((type-env (env-empty))
        (ctr (make-variable-counter)))
    (labels
        ((algorithm-j (typing f)
           (cond
             ;; Symbol
             ;; j(sym) => lookup in symbol table
             ((symbolp f)
              (if (env-bound-p f typing)
                  (let* ((x    (env-value f typing))
                         (kind (prefix-kind x))
                         (type (prefix-type x)))
                    ;; something is bugging out here.
                    (if (eql ':let kind)
                        (instance type typing ctr)
                        type))

                  ;; Not in symbol table, so must be global.
                  (instance (find-type f) (env-empty) ctr)))

             ;; Constant expression
             ;; j(constant) => constant-type
             ((not (consp f))
              (instance (constant-type f) (env-empty) ctr))

             ;; QUOTE expression
             ;; j('constant) => j(constant)
             ((eql (car f) 'quote)
              (instance (constant-type (second f)) (env-empty) ctr))

             ;; IF expression
             ;; j( if(p, x, y) ) =>
             ;;   unify(j(p), bool),
             ;;   unify(j(x), j(y))
             ((eql (car f) 'if)
              ;; Compute the type for the predicate and each branch.
              (let ((predicate-type (algorithm-j typing (second f)))
                    (conseq-type    (algorithm-j typing (third f)))
                    (alternate-type (algorithm-j typing (fourth f))))
                ;; Unify the predicate type with BOOL, and unify the
                ;; types of the branches.
                (setf type-env
                      (unify conseq-type
                             alternate-type
                             (unify predicate-type 'bool type-env)))

                ;; Return the type of either branch (as they are the
                ;; same).
                conseq-type))

             ;; LAMBDA expression
             ;; j(lambda(vars, body)) => (vars -> j(body))
             ((eql (car f) 'lambda)
              (let ((lambda-args (second f))
                    (lambda-body (third f)))
                (let* (
                       ;; Generate generic types for each parameter.
                       (param-types (mapcar (lambda (x)
                                              (declare (ignore x))
                                              (funcall ctr))
                                            lambda-args))

                       ;; Compute the type of the body expression,
                       ;; augmenting the environment with the types of
                       ;; the parameters.
                       (body-type (algorithm-j
                                   (env-join
                                    (mapcar (lambda (x y) (cons x (prefix ':lambda y)))
                                            lambda-args
                                            param-types)
                                    typing)
                                   lambda-body)))

                  ;; Construct and return the function type.
                  (cons '->
                        (list (reduce (lambda (x y) `(* ,x ,y)) param-types
                                      :from-end t)
                              body-type)))))

             ;; LET expression
             ;; j(let([[x1,y1], [x2, y2],...], body)) =>
             ;; with j(x1) := j(y1)
             ;;      j(x2) := j(y2)
             ;;      ...
             ;;      j(xn) := j(yn)
             ;;  j(body)
             ((eql (car f) 'let)
              (let ((let-bindings (second f))
                    (let-body     (third f)))
                ;; The let bindings are bound in parallel, so we can
                ;; compute the type of each bound expression
                ;; individually.
                (algorithm-j
                 (env-join
                  (mapcar (lambda (binding)
                            (cons (first binding)
                                  (prefix ':let
                                          (algorithm-j typing (second binding)))))
                          let-bindings)
                  typing)
                 let-body)))

             ;; LETREC expression
             ;; j(letrec([[x1,y1], [x2, y2],...], body)) =>
             ;; with j(x1) := T1
             ;;      j(x2) := T2
             ;;      ...
             ;;      j(xn) := Tn
             ;;  unify( j(y1), T1 )
             ;;  unify( j(y2), T2 )
             ;;  ...
             ;;  unify( j(yn), Tn )
             ;;  j(body)
             ((eql (car f) 'letrec)
              (let ((letrec-bindings (second f))
                    (letrec-body     (third f)))
                ;; First we augment our symbol table with generated
                ;; type variables for each of the bindings' variables.
                (let ((typing* (env-join
                                (mapcar (lambda (binding)
                                          (cons (first binding)
                                                (prefix ':letrec
                                                        (funcall ctr))))
                                        letrec-bindings)
                                typing)))

                  ;; Next, for each expression being bound, compute
                  ;; the type of the expression, and then augment the
                  ;; type environment by unifying the just computed
                  ;; type with our previously generated type variable.
                  (dolist (binding letrec-bindings)
                    (let ((val (algorithm-j typing* (second binding))))
                      (setf type-env
                            (unify (prefix-type (env-value (first binding)
                                                           typing*))
                                   val
                                   type-env))))

                  ;; Finally, compute the type of the body.
                  (algorithm-j typing* letrec-body))))

             ;; Lambda combination
             ;; j( (lambda([v1, v2, ..., vn], body))(a1, a2, ..., an) ) =>
             ;; j( let([[v1, a1],
             ;;         [v2, a2],
             ;;         ...,
             ;;         [vn, an]], body))
             ((and (consp (car f))
                   (eql (caar f) 'lambda))
              ;; Simply transform the lambda combination into a LET.
              (algorithm-j typing (list 'let
                                        (mapcar #'list (cadar f) (cdr f))
                                        (caddar f))))

             ;; Function combination
             ;; j( g(x1, x2, ..., xn) ) =>
             ;; unify( j(g), (j(x1), j(x2), ..., j(xn)) -> Ta)
             (t
              (let ((operator (first f))
                    (arguments (rest f)))
                (let (
                      ;; Compute the type of the operator (may be an
                      ;; arbitrary expression). Call it F.
                      (operator-type (algorithm-j typing operator))

                      ;; Compute the type of each argument. Call them (A B ...)
                      (arg-types (mapcar (lambda (x) (algorithm-j typing x))
                                         arguments))

                      ;; Generate a type variable for the entire result. Call it T.
                      (result-type (funcall ctr)))

                  ;; Unify F with (A B ...) -> T.
                  (setf type-env
                        (unify operator-type
                               (list '->
                                     (reduce (lambda (x y) `(* ,x ,y)) arg-types
                                             :from-end t)
                                     result-type)
                               type-env))

                  ;; Simply return F. This will be expanded with its
                  ;; actual type at the very end.
                  result-type))))))

      ;; Compute the type of F, and then substitute all type-variables
      ;; in.
      (let ((type-expr (algorithm-j (env-empty) f)))
        (values
         (substitute-type-variables type-expr type-env)
         type-expr
         type-env)))))
