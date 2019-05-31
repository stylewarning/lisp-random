;;;; progi.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

;;; Ideated with Mark Skilbeck and Andrew Shi.

(defmacro progi (i &body body)
  "Like PROGN, but returns the primary value of the Ith form of BODY. I is evaluated."
  (let ((count (gensym "COUNT-"))
        (expr (gensym "EXPR"))
        (return-value (gensym "RETURN-VALUE-"))
        (sentinel (gensym "SENTINEL")))
    `(let ((,count (1- ,i))
           (,return-value ',sentinel))
       ,@(loop :for form :in body
               :collect `(let ((,expr ,form))
                           (when (zerop ,count)
                             (setf ,return-value ,expr))
                           (decf ,count)))
       (when (eq ,return-value ',sentinel)
         (error "Bad PROGI."))
       ,return-value)))

(defmacro multiple-value-progi (i &body body)
  "Like PROGN, but returns the values of the Ith form of BODY. I is evaluated."
  (let ((count (gensym "COUNT-"))
        (expr (gensym "EXPR"))
        (return-value (gensym "RETURN-VALUE-"))
        (sentinel (gensym "SENTINEL")))
    `(let ((,count (1- ,i))
           (,return-value ',sentinel))
       ,@(loop :for form :in body
               :collect `(let ((,expr (multiple-value-list ,form)))
                           (when (zerop ,count)
                             (setf ,return-value ,expr))
                           (decf ,count)))
       (when (eq ,return-value ',sentinel)
         (error "Bad PROGI."))
       (values-list ,return-value))))
