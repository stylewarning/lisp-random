;;; REGISTERS
;;; 
;;; return = rax
;;; 
;;; fastcall regs
;;; -------------
;;; 
;;; rdi    xmm0
;;; rsi    ...
;;; rdx    xmm7
;;; rcx
;;; r8
;;; r9
;;; 
;;; scratch regs
;;; ------------
;;; 
;;; rax, rcx, rdx, rsi, rdi, r8 - r11
;;; 
;;; callee responsible
;;; ------------------
;;; 
;;; rbx, rsp, rbp, r12 - r15

;;; STACK
;;; 
;;; rbp = stack marker
;;; rsp = stack pointer
;;;    rsp+8n = look at stack[n]
;;;    rsp-8n = allocate n words

(defun generate-asm (expr)
  (let ((isns nil))
    (labels ((isn (&rest strs)
               (push (apply 'concatenate 'string
                            (mapcar 'princ-to-string strs))
                     isns))
             
             (gen (expr)
               (cond
                 ((integerp expr) (isn "push " expr))
                 ((atom expr) (error "unknown thing ~S" expr))
                 ((= 3 (length expr))
                  (progn
                    ;; Generate code for the first argument.
                    (gen (second expr))
                    
                    ;; Generate code for the second argument.
                    (gen (third expr))
                    
                    ;; Pop arguments off the stack into registers.
                    (isn "pop edx")
                    (isn "pop eax")
                    
                    ;; Generate arithmetic.
                    (case (car expr)
                      ((+) (isn "add eax, edx"))
                      ((-) (isn "sub eax, edx"))
                      ((*) (isn "imul eax, edx"))
                      ((/) (error "i don't know")))
                    
                    ;; Push the result back onto the stack.
                    (isn "push eax")))
                 (t (error "unknown thing ~S" expr)))))
      
      ;; Generate all the code.
      (gen expr)
      
      ;; Get result into eax.
      (isn "pop eax")
      
      ;; Return instructions.
      (nreverse isns))))
