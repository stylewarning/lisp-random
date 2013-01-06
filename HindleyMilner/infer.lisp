;;;; Hindley-Milner type inference
;;;; (c) 2010-2012 Robert Smith

;;;; This is an implementation of Robin Milner's "Algorithm J" from
;;;; his 1978 paper `A Theory of Type Polymorphism in Programming'
;;;; from the Journal of Computer and System Sciences, Volume 17,
;;;; pages 348--375. It is available (as of December 17, 2010) from
;;;; <http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.67.5276>
;;;; for free.

;;;; This implementation is geared toward a Scheme-like language. See
;;;; the file `tests.lisp' for some examples.

;;;; Also, this needs some cleaning up.


;;; Environment routines.
;;; An environment is just an association list.

(defun env-empty ()
  "Return an empty environment."
  nil)

(defun env-update (var val env)
  "Add a binding (VAR . VAL) to ENV."
  (acons var val env))

(defun env-join (env1 env2)
  "Concatenate ENV1 and ENV2"
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
  (string< (symbol-name x) (symbol-name y)))


;;; Variable-environment interaction.

(defun variable-val (var env)
  "Get the value of VAR in ENV."
  (if (and (variablep var) (env-bound-p var env))
      (variable-val (env-value var env) env)
      var))

(defun unify (x y type-env)
  "Return the values that must be substituted for the variables in X
and Y so that they unify."
  (let ((xv (variable-val x type-env))
        (yv (variable-val y type-env)))
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

(defun genericp (var symbol-table)
  "Is VAR a generic variable in SYMBOL-TABLE?"
  (cond
    ((null symbol-table) t)
    ((and (eql (cadar symbol-table) var)
          (not (eql (caar symbol-table) :let))) nil)
    (t (genericp var (cdr symbol-table)))))

(defun instance (x symbol-table counter)
  "Generate an instance of the type expression X with fresh variables
in place of the generic variables defined in SYMBOL-TABLE."
  (labels ((instance-aux (x type-env cont)
             ;; The general strategy here is to maintain table mapping
             ;; the generic type variables to their instantiated
             ;; version.
             (cond
               ((and (variablep x)
                     (genericp x symbol-table))
                (if (env-bound-p x type-env)
                    (funcall cont (env-value x type-env) type-env)
                    (let ((tyvar (funcall counter)))
                      (funcall cont tyvar (env-update x tyvar type-env)))))
               ((consp x)
                (instance-aux (car x) type-env
                              #'(lambda (a env)
                                  (instance-aux (cdr x) env
                                                #'(lambda (b env)
                                                    (funcall cont (cons a b) env))))))
               (t
                (funcall cont x type-env)))))
    (instance-aux x
                  (env-empty)
                  #'(lambda (a env)
                      (declare (ignore env))
                      a))))


;;; Inference.

(defparameter *global-var-types* (make-hash-table :test 'equal))

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
  (gethash prim *global-var-types*))

(declare-type +         (-> num num num)
              -         (-> num num num)
              *         (-> num num num)
              /         (-> num num num)
              <         (-> num num bool)
              <=        (-> num num bool)
              =         (-> num num bool)
              >=        (-> num num bool)
              char>?    (-> char char bool)
              char<?    (-> char char bool)
              char<=?   (-> char char bool)
              char=?    (-> char char bool)
              char>=?   (-> char char bool)
              char>?    (-> char char bool)
              cons      (-> Ta (list Ta) (list Ta))
              car       (-> (list Ta) Ta)
              cdr       (-> (list Ta) (list Ta))
              set-car!  (-> (list Ta) Ta ())
              set-cdr!  (-> (list Ta) (list Ta) ())
              null?     (-> (list Ta) bool)
              length    (-> (list Ta) num)
              append    (-> (list Ta) (list Ta) (list Ta))
              reverse   (-> (list Ta) (list Ta))
              map       (-> (-> Ta Tb) (list Ta) (list Tb))
              not       (-> bool bool)
              call/cc   (-> (-> (-> Ta Tb) Tc) Ta)
              apply     (-> (-> Ta Tb) (list Ta) Tb)
              display   (-> Ta ())
              write     (-> Ta ())
              true      bool
              false     bool)

(defun constant-type (x)
  "Determine the type of a constant X."
  (cond
    ((numberp x)            'num)
    ((characterp x)         'char)
    ((or (eql x 'true)
         (eql x 'false))    'bool)
    ((null x)               '(list Ta))
    ((consp x)
     (let ((element-type (constant-type (car x))))
       (mapcar #'(lambda (y)
                   (if (not (equal (constant-type y) element-type))
                       (error "List is not homogeneous.")))
               (cdr x))
       (list 'list element-type)))
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
        ((algorithm-j (symbol-table f)
           "Robin Milner's type inference algorithm, simulating his
Algorithm W."
           (cond
             ;; Symbol
             ;; j(sym) => lookup in symbol table
             ((symbolp f)
              (if (env-bound-p f symbol-table)
                  (let* ((x    (env-value f symbol-table))
                         (kind (first x))
                         (type (second x)))
                    ;; something is bugging out here.
                    (if (eql kind :let)
                        (instance type symbol-table ctr)
                        type))
                  
                  ;; Not in symbol table, so must be global.
                  ;;
                  ;; TODO: Error check here.
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
              (let ((predicate-type (algorithm-j symbol-table (second f)))
                    (conseq-type    (algorithm-j symbol-table (third f)))
                    (alternate-type (algorithm-j symbol-table (fourth f))))
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
                                    (mapcar (lambda (x y) (list x :lambda y))
                                            lambda-args
                                            param-types)
                                    symbol-table)
                                   lambda-body)))
                  
                  ;; Construct and return the function type.
                  (cons '-> (append param-types (list body-type))))))
             
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
                  (mapcar (lambda (x) 
                            (list (car x) 
                                  :let
                                  (algorithm-j symbol-table (cadr x))))
                          let-bindings)
                  symbol-table)
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
                (let ((symbol-table* (env-join
                                      (mapcar (lambda (x)
                                                (list (car x) :letrec (funcall ctr)))
                                              letrec-bindings)
                                      symbol-table)))
                  
                  ;; Next, for each expression being bound, compute
                  ;; the type of the expression, and then augment the
                  ;; type environment by unifying the just computed
                  ;; type with our previously generated type variable.
                  (dolist (binding letrec-bindings)
                    (let ((val (algorithm-j symbol-table* (cadr binding))))
                      (setf type-env
                            (unify (cadr (env-value (car binding) symbol-table*))
                                   val
                                   type-env))))
                  
                  ;; Finally, compute the type of the body.
                  (algorithm-j symbol-table* letrec-body))))

             ;; Lambda combination
             ;; j( (lambda([v1, v2, ..., vn], body))(a1, a2, ..., an) ) =>
             ;; j( let([[v1, a1],
             ;;         [v2, a2],
             ;;         ...,
             ;;         [vn, an]], body))
             ((and (consp (car f))
                   (eql (caar f) 'lambda))
              ;; Simply transform the lambda combination into a LET.
              (algorithm-j symbol-table (list 'let
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
                      (operator-type (algorithm-j symbol-table operator))

                      ;; Compute the type of each argument. Call them (A B ...)
                      (arg-types (mapcar (lambda (x) (algorithm-j symbol-table x))
                                         arguments))
                      
                      ;; Generate a type variable for the entire result. Call it T.
                      (result-type (funcall ctr)))
                  
                  ;; Unify F with (A B ...) -> T.
                  (setf type-env
                        (unify operator-type
                               (cons '-> (append arg-types (list result-type)))
                               type-env))
                  
                  ;; Simply return F. This will be expanded with its
                  ;; actual type at the very end.
                  result-type))))))

      ;; Compute the type of F, and then substitute all type-variables
      ;; in.
      (let ((type-expr (algorithm-j (env-empty) f)))
        (substitute-type-variables type-expr type-env)))))
