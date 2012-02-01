(defparameter *env* nil)

(defun evaluate (expr)
  (cond
    ((numberp expr) expr)
    ((symbolp expr) expr)
    ((consp expr) (evaluate-form expr))
    (t "BAD INPUT")))

(defun evaluate-form (expr)
  (destructuring-bind (op . args) expr
    (if (consp op)
        nil
        (case op
          ((val) (lookup (first args)))
          ((vals) (transform-vals (first args)))
          ((set) (bind (first args) (second args)))
          (t (cons op (mapcar #'evaluate args)))
          ;; ...
          ))))

(defun lookup (var)
  (let ((result (assoc var *env*)))
    (unless (null result)
      (cdr result))))

(defun bind (var val)
  (let ((result (assoc var *env*)))
    (if result
        (rplacd result val)
        (push (cons var val) *env*)))
  (values))

(defun transform-vals (expr)
  (cond
    ((numberp expr) expr)
    ((null expr) expr)
    ((symbolp expr) `(val ,expr))
    ((consp expr) (cons (transform-vals (car expr))
                        (transform-vals (cdr expr))))))

(defun repl ()
  (setf *env* nil)
  (loop
    (format t "~&> ") 
    (let ((user-input (read *standard-input* nil :quit)))
    (if (eql user-input :quit)
        (return-from repl)
        (print (evaluate user-input))))))
