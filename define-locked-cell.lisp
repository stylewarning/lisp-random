(ql:quickload :bordeaux-threads)

(defmacro define-locked-cell (name &key (initial-value nil) documentation)
  "Define a function named NAME and the setter (SETF NAME) which has locked access.

INITIAL-VALUE is the value to which (NAME) will evaluate to initially.

DOCUMENTATION is an optional documentation string for the function NAME."
  (check-type name symbol)
  (let* ((name-string (symbol-name name))
         (var (gensym (concatenate 'string name-string "-")))
         (var-lock (gensym (concatenate 'string name-string "-LOCK-")))
         (new-value (gensym "NEW-VALUE")))
    `(progn
       (defvar ,var ,initial-value)
       (defvar ,var-lock (bt:make-lock ,(concatenate 'string name-string " lock")))
       (defun ,name ()
         ,@(and documentation (list documentation))
         (bt:with-lock-held (,var-lock)
           ,var))
       (defun (setf ,name) (,new-value)
         (bt:with-lock-held (,var-lock)
           (setf ,var ,new-value))))))
