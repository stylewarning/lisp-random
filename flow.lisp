;;;; flow.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith

;;;; Postfix composition of expressions.

#+#:ignore
(flow 5
      (+ 2 $)
      (- 4 $)
      (* $ $))

(defmacro flow (expr &rest slotted-exprs)
  "Evaluate EXPR, and pass it to the next slotted expression, which itself gets evaluated.

A \"slotted expression\" is an expression which contains the symbol `$' as a free variable. It will be bound to the value previous to the slotted expression.

For example, the flow expression

    (flow 5
          (+ 2 $)
          (- 4 $)
          (* $ $))

will be evaluated as follows:

    * Evaluate 5, and substitute it into the slotted expression (+ 2 $). This results in 7.

    * Pass 7 to the slotted expression (- 4 $), and evaluate. This results in -3.

    * Pass -3 to the slotted expression (* $ $) and evaluate. This results in 9.

Similar to Clojure's \"thrush\"."
  (if (null slotted-exprs)
      expr
      (let ((g (gensym)))
        `(let ((,g ,expr))
           (declare (dynamic-extent ,g)
                    (ignorable ,g))
           (symbol-macrolet (($ ,g))
             (flow ,(car slotted-exprs) ,@(cdr slotted-exprs)))))))
