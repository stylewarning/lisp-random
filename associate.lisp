(defmacro associate (op x y &rest z)
  "The form (ASSOCIATE op x y &rest z) computes

    (and (op x y) (op y z1) (op z1 z2) ...)

but short-circuits and only computes what it needs to."
  (let* ((gens (mapcar (lambda (v)  (cons v (gensym)))
                       (list* x y z)))
         (blk (gensym "ASSOCIATE"))
         (nest (let ((x (first gens)))
                 (lambda (inner)
                   `(block ,blk
                      (let ((,(cdr x) ,(car x)))
                        ,inner))))))
    (loop :repeat (1+ (length z))
          :for (a b) :on gens
          :do (setf nest (let ((nest nest)
                               (a-symbol (cdr a))
                               (b-form (car b))
                               (b-symbol (cdr b)))
                           (lambda (inner)
                             (funcall nest
                                      `(let ((,b-symbol ,b-form))
                                         (unless (,op ,a-symbol ,b-symbol)
                                           (return-from ,blk nil))
                                         ,inner))))))
    (funcall nest `(return-from ,blk t))))
