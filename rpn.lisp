(defparameter *stack-instructions*
  ;; op-string arity symbol
  '(("+"       2     +)
    ("-"       2     -)
    ("*"       2     *)
    ("/"       2     /)
    ("^"       2     expt)
    ("neg"     1     -)
    ("abs"     1     abs)
    )
  "List of the instructions that the RPN interpreter recognizes.")

(defun split (s ss)
  "Split a string SS by a delimiter S."
  (loop
     :for n := 0 :then (+ p (length s))
     :for p := (search s ss :start2 n)
     :if p :collect (subseq ss n p)
     :else :collect (subseq ss n)
     :while p))

(defun integer-literal (s)
  "Check that a string S is an integer literal, and if so, return
it. Junk is allowed on the end of the string."
  (parse-integer s :junk-allowed t))

(defun get-instr (x)
  "Get an instruction from the instruction list. If it doesn't exist,
return NIL."
  (assoc x *stack-instructions* :test #'string-equal))

(defmacro pop-n (n stk)
  "Pop N arguments off of STK and return them in a list."
  `(loop :repeat ,n :collecting (pop ,stk)))

(defun rpn (&optional (stack nil))
  "A little, naïve RPN interpreter."
  (loop
     ;; get le input
     :for u := (progn (format t "> ") (read-line))
     ;; check if the user is tired of this BS yet
     :until (string-equal (string-downcase u) "quit")
     ;; eval the stack
     :do (dolist (i (split " " u))
           (let ((int (integer-literal i))
                 (opr (get-instr i)))
             (cond
               ;; integer
               (int (push int stack))
               ;; operator name
               (opr (push (apply (third opr)
                                 (pop-n (second opr) stack))
                          stack))
               ;; pawp 'n' print
               ((string-equal i ".") (format t "~A~%" (pop stack)))
               ;; DAMNIT
               (t (warn "undefined func: ~S" i)))))
     ;; print le stack
     (format t "top: ~{~A ~};~%" stack)
     ;; return stack when we qvit
     :finally (return stack)))

