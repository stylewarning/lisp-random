;;;; This file defines a small kernel DSL that gets
;;;; auto-vectorized on x64.

(defpackage #:nsim
  (:use #:cl)
  (:export #:define-kernel-shape
           #:define-pairwise-kernel
           #:define-self-kernel
           #:define-reduction-kernel
           #:define-kernel-step
           #:real-sqrt
           #:main))

(in-package #:nsim)


;;; Arithmetic

(declaim (inline real-sqrt)
         (ftype (function ((double-float 0.0d0)) (double-float 0.0d0))
                real-sqrt))
(defun real-sqrt (x)
  (sqrt x))

#+(and sbcl x86-64)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-simd))

#+(and sbcl x86-64)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-c:defknown nsim::%%fmadd (double-float double-float double-float)
      double-float
      (sb-c:movable sb-c:foldable sb-c:flushable)
    :overwrite-fndb-silently t)

  (sb-c:defknown nsim::%%fmsub (double-float double-float double-float)
      double-float
      (sb-c:movable sb-c:foldable sb-c:flushable)
    :overwrite-fndb-silently t)
  
  (sb-c:defknown nsim::%%fmadd-pd
      ((sb-ext:simd-pack double-float) (sb-ext:simd-pack double-float)
       (sb-ext:simd-pack double-float))
      (sb-ext:simd-pack double-float)
      (sb-c:movable sb-c:foldable sb-c:flushable)
    :overwrite-fndb-silently t)

  (sb-c:defknown nsim::%%fmsub-pd
      ((sb-ext:simd-pack double-float) (sb-ext:simd-pack double-float)
       (sb-ext:simd-pack double-float))
      (sb-ext:simd-pack double-float)
      (sb-c:movable sb-c:foldable sb-c:flushable)
    :overwrite-fndb-silently t)

  (sb-c:defknown nsim::%%f64.2-low
      ((sb-ext:simd-pack double-float)) double-float
      (sb-c:movable sb-c:foldable sb-c:flushable)
    :overwrite-fndb-silently t)

  (sb-c:defknown nsim::%%f64.2-high
      ((sb-ext:simd-pack double-float)) double-float
      (sb-c:movable sb-c:foldable sb-c:flushable)
    :overwrite-fndb-silently t)

  (sb-c:define-vop (%%fmadd-vop)
    (:translate %%fmadd)
    (:policy :fast-safe)
    (:args (acc :scs (sb-vm::double-reg) :target r)
           (a :scs (sb-vm::double-reg))
           (b :scs (sb-vm::double-reg)))
    (:arg-types double-float double-float double-float)
    (:results (r :scs (sb-vm::double-reg) :from (:argument 0)))
    (:result-types double-float)
    (:generator 4
                (sb-vm::move r acc)
                (sb-assem:inst sb-x86-64-asm::vfmadd231sd r a b)))

  (sb-c:define-vop (%%fmsub-vop)
    (:translate %%fmsub)
    (:policy :fast-safe)
    (:args (acc :scs (sb-vm::double-reg) :target r)
           (a :scs (sb-vm::double-reg))
           (b :scs (sb-vm::double-reg)))
    (:arg-types double-float double-float double-float)
    (:results (r :scs (sb-vm::double-reg) :from (:argument 0)))
    (:result-types double-float)
    (:generator 4
                (sb-vm::move r acc)
                (sb-assem:inst sb-x86-64-asm::vfnmadd231sd r a b)))

  (sb-c:define-vop (%%fmadd-pd-vop)
    (:translate %%fmadd-pd)
    (:policy :fast-safe)
    (:args (acc :scs (sb-vm::double-sse-reg) :target r)
           (a :scs (sb-vm::double-sse-reg))
           (b :scs (sb-vm::double-sse-reg)))
    (:arg-types sb-vm::simd-pack-double sb-vm::simd-pack-double
                sb-vm::simd-pack-double)
    (:results (r :scs (sb-vm::double-sse-reg) :from (:argument 0)))
    (:result-types sb-vm::simd-pack-double)
    (:generator 4
                (sb-vm::move r acc)
                (sb-assem:inst sb-x86-64-asm::vfmadd231pd r a b)))

  (sb-c:define-vop (%%fmsub-pd-vop)
    (:translate %%fmsub-pd)
    (:policy :fast-safe)
    (:args (acc :scs (sb-vm::double-sse-reg) :target r)
           (a :scs (sb-vm::double-sse-reg))
           (b :scs (sb-vm::double-sse-reg)))
    (:arg-types sb-vm::simd-pack-double sb-vm::simd-pack-double
                sb-vm::simd-pack-double)
    (:results (r :scs (sb-vm::double-sse-reg) :from (:argument 0)))
    (:result-types sb-vm::simd-pack-double)
    (:generator 4
                (sb-vm::move r acc)
                (sb-assem:inst sb-x86-64-asm::vfnmadd231pd r a b)))

  (sb-c:define-vop (%%f64.2-low-vop)
    (:translate %%f64.2-low)
    (:policy :fast-safe)
    (:args (pack :scs (sb-vm::double-sse-reg) :target r))
    (:arg-types sb-vm::simd-pack-double)
    (:results (r :scs (sb-vm::double-reg) :from (:argument 0)))
    (:result-types double-float)
    (:generator 2
                (sb-vm::move r pack)))

  (sb-c:define-vop (%%f64.2-high-vop)
    (:translate %%f64.2-high)
    (:policy :fast-safe)
    (:args (pack :scs (sb-vm::double-sse-reg)))
    (:arg-types sb-vm::simd-pack-double)
    (:results (r :scs (sb-vm::double-reg)))
    (:result-types double-float)
    (:generator 3
                (sb-assem:inst sb-x86-64-asm::vpermilpd r pack 1))))

#-(and sbcl x86-64)
(declaim (inline %%fmadd %%fmsub))

(defun %%fmadd (a b c)
  (declare (type double-float a b c))
  (+ a (* b c)))

(defun %%fmsub (a b c)
  (declare (type double-float a b c))
  (- a (* b c)))

#+(and sbcl x86-64)
(defun %%fmadd-pd (a b c)
  (declare (type (sb-ext:simd-pack double-float) a b c)
           (optimize (speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0)))
  (sb-simd-fma:f64.2-fmadd b c a))

#+(and sbcl x86-64)
(defun %%fmsub-pd (a b c)
  (declare (type (sb-ext:simd-pack double-float) a b c)
           (optimize (speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0)))
  (sb-simd-fma:f64.2-fnmadd b c a))

#+(and sbcl x86-64)
(defun %%f64.2-low (pack)
  (declare (type (sb-ext:simd-pack double-float) pack)
           (optimize (speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0)))
  (multiple-value-bind (lo hi) (sb-simd-fma:f64.2-values pack)
    (declare (ignore hi))
    lo))

#+(and sbcl x86-64)
(defun %%f64.2-high (pack)
  (declare (type (sb-ext:simd-pack double-float) pack)
           (optimize (speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0)))
  (multiple-value-bind (lo hi) (sb-simd-fma:f64.2-values pack)
    (declare (ignore lo))
    hi))

(defmacro fma-incf (place a b)
  `(setf ,place (%%fmadd ,place ,a ,b)))

(defmacro fma-decf (place a b)
  `(setf ,place (%%fmsub ,place ,a ,b)))


;;; Kernel Shapes

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct shape-info
    (name nil :type symbol)
    (count 0 :type fixnum)
    (fields nil :type list)
    (struct-name nil :type symbol))

  (defvar *shape-registry* (make-hash-table :test 'eq))

  (defun get-shape (name)
    (or (gethash name *shape-registry*)
        (error "Unknown kernel shape: ~S" name))))

(defmacro define-kernel-shape (name count &body fields)
  (let* ((struct-name (intern (format nil "~A-SYSTEM" name)))
         (ctor (intern (format nil "MAKE-~A-SYSTEM" name)))
         (internal-ctor (intern (format nil "%MAKE-~A-SYSTEM" name)))
         (field-list (if (and (= 1 (length fields)) (listp (first fields)))
                         (first fields)
                         fields)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (gethash ',name *shape-registry*)
               (make-shape-info :name ',name
                                :count ,count
                                :fields ',field-list
                                :struct-name ',struct-name)))
       (defstruct (,struct-name (:constructor ,internal-ctor)
                                (:conc-name ,(intern (format nil "~A-SYSTEM-" name))))
         ,@(loop :for f :in field-list
                 :collect `(,f (make-array ,count :element-type 'double-float
                                                  :initial-element 0d0)
                               :type (simple-array double-float (,count))
                               :read-only t)))
       (defun ,ctor (&rest bodies)
         (let ((sys (,internal-ctor)))
           (loop :for body :in bodies
                 :for idx :of-type fixnum :from 0
                 :do ,@(loop :for f :in field-list
                             :for kw := (intern (string f) ':keyword)
                             :collect `(setf (aref (,(intern (format nil "~A-SYSTEM-~A" name f)) sys) idx)
                                             (coerce (getf body ,kw) 'double-float))))
           sys))
       ',name)))


;;; Kernel Definition Macros

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *kernel-bodies* (make-hash-table :test 'eq))

  (defun %field-bindings (fields shape-name sys-var)
    (loop :for f :in fields
          :collect `(,(intern (format nil "$~A" f))
                     (,(intern (format nil "~A-SYSTEM-~A" shape-name f)) ,sys-var))))

  (defun %field-decls (fields count)
    `((declare (type (simple-array double-float (,count))
                     ,@(loop :for f :in fields
                             :collect (intern (format nil "$~A" f))))
               (ignorable ,@(loop :for f :in fields
                                  :collect (intern (format nil "$~A" f)))))))

  (defun %element-bindings (prefix fields idx)
    (loop :for f :in fields
          :collect `(,(intern (format nil "~A.~A" prefix f))
                     (aref ,(intern (format nil "$~A" f)) ,idx))))

  (defun %all-pairs (n)
    (loop :for i :below n
          :append (loop :for j :from (1+ i) :below n
                        :collect (list i j))))

  (defun %field-from-accessor (sym prefix)
    "If SYM is PREFIX.FIELD (e.g., I.X when prefix is I), return FIELD symbol."
    (let* ((name (symbol-name sym))
           (pstr (concatenate 'string (string prefix) "."))
           (plen (length pstr)))
      (when (and (> (length name) plen)
                 (string= pstr name :end2 plen))
        (intern (subseq name plen)))))

  (defun %find-mutated-i-fields (body)
    "Scan kernel body for i.FIELD mutations via fma-incf/fma-decf/incf/decf."
    (let ((fields nil))
      (labels ((walk (form)
                 (when (consp form)
                   (cond
                     ((member (car form) '(fma-incf fma-decf incf decf))
                      (let ((f (%field-from-accessor (second form) 'i)))
                        (when (and f (not (member f fields)))
                          (push f fields))))
                     (t
                      (dolist (sub (cdr form)) (walk sub))))
                   )))
        (dolist (form body)
          (walk form)))
      (nreverse fields)))

  (defun %scalar-fma-expr (expr)
    "Rewrite

    (+ a (* b c))    -> (%%fmadd a b c)
    (- a (* b c))    -> (%%fmsub a b c)
    (incf p (* a b)) -> (setf p (%%fmadd p a b))
    (decf p (* a b)) -> (setf p (%%fmsub p a b))."
    (if (atom expr)
        expr
        (flet ((mul-p (e)
                 (and (consp e)
                      (eq '* (car e))
                      (= (length (cdr e)) 2))))
          (case (car expr)
            ((+)
             (let ((args (mapcar #'%scalar-fma-expr (cdr expr))))
               (cond
                 ((and (= (length args) 2) (mul-p (second args)))
                  `(%%fmadd ,(first args) ,(second (second args)) ,(third (second args))))
                 ((and (= (length args) 2) (mul-p (first args)))
                  `(%%fmadd ,(second args) ,(second (first args)) ,(third (first args))))
                 (t `(+ ,@args)))))
            ((-)
             (let ((args (mapcar #'%scalar-fma-expr (cdr expr))))
               (if (and (= (length args) 2) (mul-p (second args)))
                   `(%%fmsub ,(first args) ,(second (second args)) ,(third (second args)))
                   `(- ,@args))))
            ((incf)
             (let ((delta (%scalar-fma-expr (or (third expr) 1))))
               (if (mul-p delta)
                   `(setf ,(second expr) (%%fmadd ,(second expr) ,(second delta) ,(third delta)))
                   `(incf ,(second expr) ,delta))))
            ((decf)
             (let ((delta (%scalar-fma-expr (or (third expr) 1))))
               (if (mul-p delta)
                   `(setf ,(second expr) (%%fmsub ,(second expr) ,(second delta) ,(third delta)))
                   `(decf ,(second expr) ,delta))))
            ((let let*)
             `(,(car expr)
               ,(mapcar (lambda (b)
                          (if (and (consp b) (cdr b))
                              (list (first b) (%scalar-fma-expr (second b)))
                              b))
                        (second expr))
               ,@(mapcar #'%scalar-fma-expr (cddr expr))))
            ((the)
             `(the ,(second expr) ,(%scalar-fma-expr (third expr))))
            ((declare)
             expr)
            (t
             (cons (car expr) (mapcar #'%scalar-fma-expr (cdr expr))))))))

  (defun %scalar-fma-body (body)
    (mapcar #'%scalar-fma-expr body))

  (defun %expand-pairwise (body fields pairs)
    (let ((fma-body (%scalar-fma-body body)))
      (loop :for (i j) :in pairs
            :collect `(symbol-macrolet
                          (,@(%element-bindings 'i fields i)
                           ,@(%element-bindings 'j fields j))
                        ,@fma-body))))

  (defun %expand-pairwise-nested (body fields n)
    "Nested pairwise: body i in registers across all j, store back mutable."
    (let ((fma-body (%scalar-fma-body body))
          (i-locals (mapcar (lambda (f) (intern (format nil "I-~A" f))) fields))
          (mutable (%find-mutated-i-fields body)))
      (loop :for i :below (1- n)
            :collect
            `(let (,@(loop :for f :in fields
                           :for loc :in i-locals
                           :collect `(,loc (aref ,(intern (format nil "$~A" f)) ,i))))
               (declare (type double-float ,@i-locals))
               ,@(loop :for j :from (1+ i) :below n
                       :collect
                       `(symbol-macrolet
                            (,@(loop :for f :in fields
                                     :for loc :in i-locals
                                     :collect `(,(intern (format nil "I.~A" f)) ,loc))
                             ,@(%element-bindings 'j fields j))
                          ,@fma-body))
               ,@(loop :for f :in fields
                       :for loc :in i-locals
                       :when (member f mutable)
                         :collect `(setf (aref ,(intern (format nil "$~A" f)) ,i) ,loc))))))

  (defun %expand-self (body fields n)
    (let ((fma-body (%scalar-fma-body body)))
      (loop :for i :below n
            :collect `(symbol-macrolet (,@(%element-bindings 'self fields i))
                        ,@fma-body))))

  (defun %expand-reduction-clauses (clauses)
    (let (self-body pair-body)
      (dolist (clause clauses)
        (ecase (first clause)
          ((:self) (setf self-body (%scalar-fma-body (rest clause))))
          ((:pair) (setf pair-body (%scalar-fma-body (rest clause))))))
      (values self-body pair-body))))

#+(and sbcl x86-64)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %flatten-kernel-body (forms)
    "Extract (values bindings i-mutations j-mutations) from kernel
body. Walks let/let* forms, skips declares, collects
mutations. Normalizes (incf p (* a b)) -> (fma-incf p a b) etc."
    (let (bindings i-muts j-muts)
      (labels ((push-mut (form)
                 (if (%field-from-accessor (second form) 'i)
                     (push form i-muts)
                     (push form j-muts)))
               (walk (forms)
                 (dolist (form forms)
                   (when (consp form)
                     (case (car form)
                       ((let let*)
                        (dolist (b (second form))
                          (push (if (consp b) b (list b nil)) bindings))
                        (walk (cddr form)))
                       (declare nil)
                       ((fma-incf fma-decf) (push-mut form))
                       ((incf decf)
                        (let ((delta (third form)))
                          (when (and (consp delta) (eq '* (car delta))
                                     (= (length (cdr delta)) 2))
                            (push-mut `(,(if (eq (car form) 'incf) 'fma-incf 'fma-decf)
                                        ,(second form) ,(second delta) ,(third delta))))))
                       (t nil))))))
        (walk forms))
      (values (nreverse bindings) (nreverse i-muts) (nreverse j-muts))))

  (defun %simdify-expr (expr locals extra-args j0)
    "Transform a scalar double-float expression to packed-double SIMD."
    (cond
      ((symbolp expr)
       (let ((i-field (%field-from-accessor expr 'i))
             (j-field (%field-from-accessor expr 'j)))
         (cond
           ((member expr locals :test #'eq)
            (intern (format nil "~A.2" (symbol-name expr))))
           (i-field
            `(sb-simd-fma:f64.2-broadcast
              ,(intern (format nil "I-~A" (symbol-name i-field)))))
           (j-field
            `(sb-simd-fma:f64.2-aref
              ,(intern (format nil "$~A" (symbol-name j-field))) ,j0))
           ((member expr extra-args :test #'eq)
            `(sb-simd-fma:f64.2-broadcast ,expr))
           (t expr))))
      ((consp expr)
       (flet ((xf (e) (%simdify-expr e locals extra-args j0))
              (mul-p (e) (and (consp e) (eq '* (car e)) (= (length (cdr e)) 2))))
         (case (car expr)
           ;; (+ a (* b c)) -> fmadd(a,b,c)
           ;; (+ (* b c) a) -> fmadd(a,b,c)
           ((+)
            (let ((args (cdr expr)))
              (cond
                ((and (= (length args) 2) (mul-p (second args)))
                 `(%%fmadd-pd ,(xf (first args))
                              ,(xf (second (second args)))
                              ,(xf (third (second args)))))
                ((and (= (length args) 2) (mul-p (first args)))
                 `(%%fmadd-pd ,(xf (second args))
                              ,(xf (second (first args)))
                              ,(xf (third (first args)))))
                (t `(sb-simd-fma:f64.2+ ,@(mapcar #'xf args))))))
           ;; (- a (* b c)) -> fmsub(a,b,c)
           ((-)
            (let ((args (cdr expr)))
              (if (and (= (length args) 2) (mul-p (second args)))
                  `(%%fmsub-pd ,(xf (first args))
                               ,(xf (second (second args)))
                               ,(xf (third (second args))))
                  `(sb-simd-fma:f64.2- ,@(mapcar #'xf args)))))
           ((*)
            `(sb-simd-fma:f64.2* ,@(mapcar #'xf (cdr expr))))
           ((/)
            `(sb-simd-fma:f64.2/ ,@(mapcar #'xf (cdr expr))))
           ((sqrt real-sqrt)
            `(sb-simd-fma:f64.2-sqrt ,@(mapcar #'xf (cdr expr))))
           ((%%fmadd)
            `(%%fmadd-pd ,@(mapcar #'xf (cdr expr))))
           ((%%fmsub)
            `(%%fmsub-pd ,@(mapcar #'xf (cdr expr))))
           ((the)
            (xf (third expr)))
           (t
            expr))))
      ((numberp expr)
       `(sb-simd-fma:f64.2-broadcast ,(coerce expr 'double-float)))
      (t expr)))

  (defun %lane-ref (sym lane locals)
    "Map a mutation argument to its lane-extracted form (lane 0 or 1)."
    (if (member sym locals :test #'eq)
        (intern (format nil "~A.~D" (symbol-name sym) lane))
        sym))

  (defun %simd-pair-block (body extra-args j0)
    "Generate SIMD pair block by transforming the user's pairwise kernel
body. Produces packed-double computation, lane extraction for
i-mutations, and SIMD load-FMA-store for j-mutations."
    (multiple-value-bind (bindings i-muts j-muts)
        (%flatten-kernel-body body)
      (let ((locals '())
            (simd-bindings '()))
        ;; Transform each binding to packed-double
        (dolist (b bindings)
          (let* ((name (first b))
                 (value (second b))
                 (s-name (intern (format nil "~A.2" (symbol-name name))))
                 (s-value (%simdify-expr value locals extra-args j0)))
            (push name locals)
            (push (list s-name s-value) simd-bindings)))
        (setf simd-bindings (nreverse simd-bindings)
              locals (nreverse locals))
        ;; Collect unique locals needing lane extraction for i-mutations
        (let* ((extract-vars
                 (let (vars)
                   (dolist (mut i-muts (nreverse vars))
                     (dolist (arg (cddr mut))
                       (when (and (member arg locals :test #'eq)
                                  (not (member arg vars :test #'eq)))
                         (push arg vars))))))
               (extract-binds
                 (loop :for v :in extract-vars
                       :for nm := (symbol-name v)
                       :collect `(,(intern (format nil "~A.0" nm))
                                  (%%f64.2-low ,(intern (format nil "~A.2" nm))))
                       :collect `(,(intern (format nil "~A.1" nm))
                                  (%%f64.2-high ,(intern (format nil "~A.2" nm))))))
               (i-forms
                 (loop :for (op target a b) :in i-muts
                       :for fld := (symbol-name (%field-from-accessor target 'i))
                       :for iloc := (intern (format nil "I-~A" fld))
                       :collect `(,op ,iloc ,(%lane-ref a 0 locals)
                                      ,(%lane-ref b 0 locals))
                       :collect `(,op ,iloc ,(%lane-ref a 1 locals)
                                      ,(%lane-ref b 1 locals))))
               (j-forms
                 (loop :for (op target a b) :in j-muts
                       :for fld := (symbol-name (%field-from-accessor target 'j))
                       :for arr := (intern (format nil "$~A" fld))
                       :for sop := (ecase op
                                     (fma-incf '%%fmadd-pd)
                                     (fma-decf '%%fmsub-pd))
                       :collect `(setf (sb-simd-fma:f64.2-aref ,arr ,j0)
                                       (,sop (sb-simd-fma:f64.2-aref ,arr ,j0)
                                             ,(%simdify-expr a locals extra-args j0)
                                             ,(%simdify-expr b locals extra-args j0))))))
          `(let* (,@simd-bindings)
             (declare (type (sb-ext:simd-pack double-float)
                            ,@(mapcar #'first simd-bindings)))
             ,@(when i-forms
                 `((let (,@extract-binds)
                     (declare (type double-float
                                    ,@(mapcar #'first extract-binds)))
                     ,@i-forms)))
             ,@j-forms)))))

  (defun %expand-pairwise-nested-simd (body fields n extra-args)
    "SIMD pairwise: process consecutive j-pairs with f64.2 packed-double
ops. Falls back to scalar for leftover odd j."
    (let ((i-locals (mapcar (lambda (f) (intern (format nil "I-~A" f))) fields))
          (mutable (%find-mutated-i-fields body)))
      (loop :for i :below (1- n)
            :collect
            (let* ((j-start (1+ i))
                   (j-count (- n j-start))
                   (simd-pairs (floor j-count 2))
                   (has-tail (oddp j-count)))
              `(let (,@(loop :for f :in fields
                             :for loc :in i-locals
                             :collect `(,loc (aref ,(intern (format nil "$~A" f)) ,i))))
                 (declare (type double-float ,@i-locals))
                 ;; SIMD pairs
                 ,@(loop :for p :below simd-pairs
                         :for j0 := (+ j-start (* p 2))
                         :collect (%simd-pair-block body extra-args j0))
                 ;; Scalar tail (if odd number of j's)
                 ,@(when has-tail
                     (let ((j-last (1- n)))
                       `((symbol-macrolet
                             (,@(loop :for f :in fields
                                      :for loc :in i-locals
                                      :collect `(,(intern (format nil "I.~A" f)) ,loc))
                              ,@(%element-bindings 'j fields j-last))
                           ,@(%scalar-fma-body body)))))
                 ;; Store back mutated i-fields
                 ,@(loop :for f :in fields
                         :for loc :in i-locals
                         :when (member f mutable)
                           :collect `(setf (aref ,(intern (format nil "$~A" f)) ,i) ,loc)))))))) ; eval-when

(defmacro define-pairwise-kernel (name (sys-var shape-name &rest extra-args) &body body)
  (let* ((info (get-shape shape-name))
         (n (shape-info-count info))
         (fields (shape-info-fields info))
         (struct-name (shape-info-struct-name info))
         (pairs (%all-pairs n)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (gethash ',name *kernel-bodies*)
               (list :kind :pairwise :shape ',shape-name
                     :extra-args ',extra-args :body ',body)))
       (declaim (inline ,name))
       (defun ,name (,sys-var ,@extra-args)
         (declare (optimize (speed 3) (safety 0) (debug 0)
                            (compilation-speed 0) (space 0))
                  (type ,struct-name ,sys-var)
                  ,@(when extra-args `((type double-float ,@extra-args))))
         (let (,@(%field-bindings fields shape-name sys-var))
           ,@(%field-decls fields n)
           ,@(%expand-pairwise body fields pairs)
           (values))))))

(defmacro define-self-kernel (name (sys-var shape-name &rest extra-args) &body body)
  (let* ((info (get-shape shape-name))
         (n (shape-info-count info))
         (fields (shape-info-fields info))
         (struct-name (shape-info-struct-name info)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (gethash ',name *kernel-bodies*)
               (list ':kind ':self ':shape ',shape-name
                     ':extra-args ',extra-args ':body ',body)))
       (declaim (inline ,name))
       (defun ,name (,sys-var ,@extra-args)
         (declare (optimize (speed 3) (safety 0) (debug 0)
                            (compilation-speed 0) (space 0))
                  (type ,struct-name ,sys-var)
                  ,@(when extra-args `((type double-float ,@extra-args))))
         (let (,@(%field-bindings fields shape-name sys-var))
           ,@(%field-decls fields n)
           ,@(%expand-self body fields n)
           (values))))))

(defmacro define-reduction-kernel ((name acc-var init-val) (sys-var shape-name)
                                   &body clauses)
  (let* ((info (get-shape shape-name))
         (n (shape-info-count info))
         (fields (shape-info-fields info))
         (struct-name (shape-info-struct-name info))
         (pairs (%all-pairs n)))
    (multiple-value-bind (self-body pair-body)
        (%expand-reduction-clauses clauses)
      `(defun ,name (,sys-var)
         (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0))
                  (type ,struct-name ,sys-var))
         (let (,@(%field-bindings fields shape-name sys-var))
           ,@(%field-decls fields n)
           (let ((,acc-var ,init-val))
             (declare (type double-float ,acc-var))
             ,@(when self-body
                 (loop :for i :below n
                       :collect `(setf ,acc-var
                                       (symbol-macrolet (,@(%element-bindings 'self fields i))
                                         ,@self-body))))
             ,@(when pair-body
                 (loop :for (i j) :in pairs
                       :collect `(setf ,acc-var
                                       (symbol-macrolet (,@(%element-bindings 'i fields i)
                                                         ,@(%element-bindings 'j fields j))
                                         ,@pair-body))))
             ,acc-var))))))

(defmacro define-kernel-step (name (sys-var shape-name n-var
                                    &key params)
                              &body calls)
  (let* ((info (get-shape shape-name))
         (n (shape-info-count info))
         (fields (shape-info-fields info))
         (struct-name (shape-info-struct-name info))
         (param-names (mapcar #'first params))
         (param-decls (loop :for (pn pt) :in params :collect `(type ,pt ,pn))))
    `(defun ,name (,n-var ,sys-var ,@param-names)
       (declare (optimize (speed 3) (safety 0) (debug 0)
                          (compilation-speed 0) (space 0))
                (type fixnum ,n-var)
                (type ,struct-name ,sys-var)
                ,@param-decls)
       (let (,@(%field-bindings fields shape-name sys-var))
         ,@(%field-decls fields n)
         (dotimes (,(gensym) ,n-var)
           ,@(loop :for call :in calls
                   :append
                   (let* ((kname (first call))
                          (args (rest call))
                          (ki (or (gethash kname *kernel-bodies*)
                                  (error "Unknown kernel: ~A" kname)))
                          (kind (getf ki ':kind))
                          (extra-args (getf ki ':extra-args))
                          (body (getf ki ':body)))
                     `((let (,@(mapcar #'list extra-args args))
                         ,@(when extra-args
                             `((declare (type double-float ,@extra-args))))
                         ,@(ecase kind
                             ((:pairwise)
                              #+(and sbcl x86-64)
                              (%expand-pairwise-nested-simd body fields n
                                                            extra-args)
                              #-(and sbcl x86-64)
                              (%expand-pairwise-nested body fields n))
                             ((:self) (%expand-self body fields n))))))))
         (values)))))
