;;;; let-alist.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

(defmacro let-alist ((vars &key (not-found :not-found)
                                (value ''cdr))
                     alist
                     &body body)
  "Within the body BODY, bind the variables VARS to values looked up in the association list ALIST whose keys are keywords with the same names as VARS. If the key to ALIST does not exist, bind the variable to NOT-FOUND, which is the keyword :NOT-FOUND by default. Association list values are accessed by the function VALUE, which is CL:CDR by default."
  (assert (every #'symbolp vars) ()
          "The variables must be symbols, but a non-symbol was encountered: ~S"
          (find-if-not #'symbolp vars))
  
  (let ((alist-once (gensym "ALIST-ONCE-"))
        (value-once (gensym "VALUE-ONCE-"))
        (not-found-once (gensym "NOT-FOUND-ONCE-"))
        (found (gensym "FOUND-")))
    (flet ((lookup-form (var)
             `(let ((,found (assoc ,(intern (symbol-name var) :keyword) ,alist-once)))
                (if ,found
                    (funcall ,value-once ,found)
                    ,not-found-once))))
      `(let ((,alist-once ,alist)
             (,value-once ,value)
             (,not-found-once ,not-found))
         (let ,(mapcar (lambda (var)
                         (list var (lookup-form var)))
                vars)
           ,@body)))))


;; CL-USER> (let-alist ((a b c))
;;                     '((:a . 1) (:c . 3))
;;            (format t "A = ~S~%" a)
;;            (format t "B = ~S~%" b)
;;            (format t "C = ~S~%" c))
;; A = 1
;; B = :NOT-FOUND
;; C = 3
;; NIL
;; CL-USER> (let-alist ((a b c) :value 'second)
;;                     '((:a 1) (:c 3))
;;            (format t "A = ~S~%" a)
;;            (format t "B = ~S~%" b)
;;            (format t "C = ~S~%" c))
;; A = 1
;; B = :NOT-FOUND
;; C = 3
;; NIL
;; CL-USER> (let-alist ((a b c) :value 'second :not-found nil)
;;                     '((:a 1) (:c 3))
;;            (format t "A = ~S~%" a)
;;            (format t "B = ~S~%" b)
;;            (format t "C = ~S~%" c))
;; A = 1
;; B = NIL
;; C = 3
;; NIL
