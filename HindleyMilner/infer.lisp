;;;; Hindley-Milner type inference
;;;; (c) 2010 Robert Smith

;;;; This is an implementation of Robin Milner's "Algorithm J" from
;;;; his 1978 paper `A Theory of Type Polymorphism in Programming'
;;;; from the Journal of Computer and System Sciences, Volume 17,
;;;; pages 348--375. It is available (as of December 17, 2010) from
;;;; <http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.67.5276>
;;;; for free.

;;;; This implementation is geared toward a Scheme-like language. See
;;;; the file `tests.lisp' for some examples.

;;;; Also, this needs some cleaning up.


;;; Some error conditions used in the inference.

(define-condition :cannot-unify-structures-error (error)
  ())

(define-condition :unknown-constant-type-error (error)
  ())

(define-condition :list-not-homogeneous-error (error)
  ())


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

(defparameter *variable-count* 0 "A counter for generating type variables.")

(defun new-variable ()
  "Generate a new generic type variable. A type variable will start
with a capitalized letter T followed by an integer."
  (incf *variable-count*)
  (intern (concatenate 'string "T" (write-to-string *variable-count*))))

(defun variablep (x)
  "Check if X is a type variable."
  (and (symbolp x)
       (char= (aref (symbol-name x) 0) #\T)))

(defun variable< (x y)
  "Check if variable X was generated before Y."
  (string< (symbol-name x) (symbol-name y)))


;;; Variable-environment interaction.

(defun variable-val (var env)
  "Get the value of VAR in ENV."
  (if (and (variablep var) (env-bound-p var env))
      (variable-val (env-value var env) env)
      var))

(defun unify (x y env)
  "Return the values that must be substituted for the variables in X
and Y so that they unify."
  (let ((xv (variable-val x env))
        (yv (variable-val y env)))
    (cond
      ((equalp xv yv)
       env)
      ((and (variablep xv) (or (not (variablep yv)) (variable< yv xv)))
       (env-update xv yv env))
      ((variablep yv)
       (env-update yv xv env))
      ((and (consp xv) (consp yv))
       (unify (cdr xv) (cdr yv) (unify (car xv) (car yv) env)))
      (t
       (error :cannot-unify-structures-error)))))

(defun substitute-values (x env)
  "Return X but with each variable in X substituted for the values
defined in ENV."
  ;; Maybe just use SUBLIS?
  (cond ((variablep x) (let ((y (variable-val x env)))
                         (if (variablep y) y (substitute-values y env))))
        ((consp x) (cons (substitute-values (car x) env)
                         (substitute-values (cdr x) env)))
        (t x)))

(defun instance-aux (x prefix env success)
    "Generate an instance of X with fresh variables in place of the
generic variables defined in PREFIX. Finally, call SUCCESS on the
generated instance. This is used as an auxiliary routine for
INSTANCE."
  (cond
    ((and (variablep x) (genericp x prefix))
     (if (env-bound-p x env)
         (funcall success (env-value x env) env)
         (let ((var (new-variable)))
           (funcall success var (env-update x var env)))))
    ((consp x)
     (instance-aux (car x) prefix env
          #'(lambda (a env)
              (instance-aux (cdr x) prefix env
                   #'(lambda (b env)
                       (funcall success (cons a b) env))))))
    (t
     (funcall success x env))))

(defun instance (x prefix)
  "Generate an instance of X with fresh variables in place of the
generic variables defined in PREFIX."
  (instance-aux x
                prefix
                (env-empty)
                #'(lambda (a env)
                    (declare (ignore env))
                    a)))

(defun genericp (var prefix)
  "Is VAR a generic variable in PREFIX?"
  (cond
    ((null prefix) t)
    ((and (equalp (cadar prefix) var) (not (equalp (caar prefix) 'let))) nil)
    (t (genericp var (cdr prefix)))))


;;; Inference.

(defvar global-var-types
  '(
    (+        . (-> num num num))
    (-        . (-> num num num))
    (*        . (-> num num num))
    (/        . (-> num num num))
    (<        . (-> num num bool))
    (<=       . (-> num num bool))
    (=        . (-> num num bool))
    (>=       . (-> num num bool))
    (char>?   . (-> char char bool))
    (char<?   . (-> char char bool))
    (char<=?  . (-> char char bool))
    (char=?   . (-> char char bool))
    (char>=?  . (-> char char bool))
    (char>?   . (-> char char bool))
    (cons     . (-> Ta (list Ta) (list Ta)))
    (car      . (-> (list Ta) Ta))
    (cdr      . (-> (list Ta) (list Ta)))
    (set-car! . (-> (list Ta) Ta ()))
    (set-cdr! . (-> (list Ta) (list Ta) ()))
    (null?    . (-> (list Ta) bool))
    (length   . (-> (list Ta) num))
    (append   . (-> (list Ta) (list Ta) (list Ta)))
    (reverse  . (-> (list Ta) (list Ta)))
    (map      . (-> (-> Ta Tb) (list Ta) (list Tb)))
    (not      . (-> bool bool))
    (call/cc  . (-> (-> (-> Ta Tb) Tc) Ta))
    (apply    . (-> (-> Ta Tb) (list Ta) Tb))
    (write    . (-> Ta ()))
    )
  "A list of types for some common Scheme procedures."
  )

(defun constant-type (x)
  "Determine the type of a constant X."
  (cond
    ((numberp x)                     'num)
    ((characterp x)                  'char)
    ((or (equalp x 'true) (equalp x 'false)) 'bool)
    ((null x)                        '(list Ta))
    ((consp x)
     (let ((element-type (constant-type (car x))))
       (mapcar #'(lambda (y)
                   (if (not (equal (constant-type y) element-type))
                       (error :list-not-homogeneous-error)))
               (cdr x))
       (list 'list element-type)))
    (t (error :unknown-constant-type-error))))

(defun derive-type (f)
  "Derive the type of expression F and return it using Milner's Algorithm J."
  (let ((e (env-empty)))
    (labels
        ((algorithm-j (p f)
           "Robin Milner's type inference algorithm, simulating his
Algorithm W."
           (cond
             ((symbolp f)
              ;; j(Sym) => j(val(Sym))
              (if (env-bound-p f p)
                  (let* ((x (env-value f p))
                         (kind (car x))
                         (derive-type (cadr x)))
                    (if (equalp kind 'let)
                        (instance derive-type p)
                        derive-type))
                  (instance (cdr (assoc f global-var-types)) (env-empty))))
             ((not (consp f))
              ;; j(constant) => constant-type
              (instance (constant-type f) (env-empty)))
             ((equalp (car f) 'quote)
              ;; j('constant) => j(constant)
              (instance (constant-type (cadr f)) (env-empty)))
             ((equalp (car f) 'if)
              ;; j( if(p, x, y) ) =>
              ;;   unify(j(p), bool),
              ;;   unify(j(x), j(y))
              (let ((pre (algorithm-j p (cadr f)))
                    (con (algorithm-j p (caddr f)))
                    (alt (algorithm-j p (cadddr f))))
                (setf e (unify con alt (unify pre 'bool e)))
                con))
             ((equalp (car f) 'lambda)
              ;; j(lambda(vars, body)) => (vars -> j(body))
              (let* ((parms (mapcar (lambda (x) (new-variable)) (cadr f)))
                     (body (algorithm-j
                            (env-join
                             (mapcar (lambda (x y) (list x 'lambda y))
                                     (cadr f)
                                     parms)
                             p)
                            (caddr f))))
                (cons '-> (append parms (list body)))))
             ((equalp (car f) 'let)
              ;; j(let([[x1,y1], [x2, y2],...], body)) =>
              ;; with j(x1) := j(y1)
              ;;      j(x2) := j(y2)
              ;;      ...
              ;;      j(xn) := j(yn)
              ;;  j(body)
              (algorithm-j
               (env-join
                (mapcar
                 (lambda (x) (list (car x) 'let (algorithm-j p (cadr x))))
                 (cadr f))
                p)
               (caddr f)))
             ((equalp (car f) 'letrec)
              ;; j(letrec([[x1,y1], [x2, y2],...], body)) =>
              ;; with j(x1) := T1
              ;;      j(x2) := T2
              ;;      ...
              ;;      j(xn) := Tn
              ;;  unify( j(y1), j(y2), ..., j(yn)),
              ;;  j(body)
              (let ((p* (env-join
                         (mapcar (lambda (x)
                                   (list (car x) 'letrec (new-variable)))
                                 (cadr f))
                         p)))
                (mapcar
                 (lambda (x)
                   (let ((val (algorithm-j p* (cadr x))))
                     (setf e (unify (cadr (env-value (car x) p*)) val e))))
                 (cadr f))
                (algorithm-j p* (caddr f))))
             ((and (consp (car f)) (equalp (caar f) 'lambda))
              ;; j( (lambda([v1, v2, ..., vn], body))(a1, a2, ..., an) ) =>
              ;; j( let([[v1, a1],
              ;;         [v2, a2],
              ;;         ...,
              ;;         [vn, an]], body))
              (algorithm-j p (list 'let
                                   (mapcar #'list (cadar f) (cdr f))
                                   (caddar f))))
             (t
              ;; j( g(x1, x2, ..., xn) ) =>
              ;; unify( j(g), (j(x1), j(x2), ..., j(xn)) -> Ta)
              (let ((result (new-variable))
                    (oper (algorithm-j p (car f)))
                    (args (mapcar (lambda (x) (algorithm-j p x)) (cdr f))))
                (setf e (unify oper (cons '-> (append args (list result))) e))
                result)))))
      ;; Compute the type of f, and then substitute all
      ;; type-variables in.
      (let ((tt (algorithm-j (env-empty) f)))
        (substitute-values tt e)))))
