;;;; forth.lisp
;;;; Copyright (c) 2013 Robert Smith

;;;; Experiments with a Forth-like language.

(defstruct forth
  (stack nil)
  (dict (make-hash-table)))

(defun lookup-word (word state)
  (gethash word (forth-dict state)))

(defun execute-word (word state)
  (let ((code (lookup-word word state)))
    (cond
      ((functionp code) (funcall code state))
      ((consp code) (interpret code state))
      ((null code) (error "Undefined word: ~A" word))
      (t (error "Invalid word: ~A" word)))))

(defun install-primitive (state word fn)
  (setf (gethash word (forth-dict state))
        fn))

(defun primordial-forth ()
  (let ((state (make-forth)))
    (macrolet ((defprim (name stuff &body body)
                 (declare (ignore stuff))
                 `(install-primitive state ',name
                                     (lambda (state)
                                       ,@body))))
      (defprim call ()
        (let ((f (pop (forth-stack state))))
          (interpret f state)))
      
      (defprim print ()
        (let ((x (pop (forth-stack state))))
          (format t "~A" x)))
      
      (defprim + ()
        (let* ((a (pop (forth-stack state)))
               (b (pop (forth-stack state))))
          (push (+ a b) (forth-stack state)))))
    
    ;; Return the state.
    state))

(defun interpret (words state)
  (dolist (word words state)
    (cond
      ((or (numberp word)
           (consp word)
           (stringp word)) (push word (forth-stack state)))
      ((symbolp word) (execute-word word state))
      (t (error "Unrecognized token: ~S" word)))))
