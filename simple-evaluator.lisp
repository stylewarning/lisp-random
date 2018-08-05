;;;; simple-evaluator.lisp
;;;;
;;;; Copyright (c) 2018 Robert Smith

;;; Another Common Lisp version of the Haskell code from the blog post
;;;
;;;    https://z0ltan.wordpress.com/2018/08/04/simple-expression-evaluator-comparison-between-haskell-rust-and-common-lisp/

(ql:quickload :algebraic-data-library)

(adt:defdata op add sub mul div)

(adt:defdata expr
  (val integer)
  (app op expr expr))

(defun ev (e)
  (adt:match expr e
    ((val n) (adl:just n))
    ((app o e1 e2)
     (adl:mlet ((v1 (ev e1))
                (v2 (ev e2)))
       (adt:match op o
         (add (adl:just (+ v1 v2)))
         (sub (adl:just (- v1 v2)))
         (mul (adl:just (* v1 v2)))
         (div (if (zerop v2)
                  adl:nothing
                  (adl:just (floor v1 v2)))))))))

(defvar *e1* (app add (val 2) (app mul (val 3) (val 6))))
(defvar *e2* (app mul (app add (val 1) (val 3)) (app div (val 10) (val 0))))

(defun main ()
  (print (ev *e1*))   ; => #.(JUST 20)
  (print (ev *e2*)))  ; => #.NOTHING
