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
                      ;; +-* all are easy.
                      ((+) (isn "add eax, edx"))
                      ((-) (isn "sub eax, edx"))
                      ((*) (isn "imul eax, edx"))
                      
                      ;; division requires
                      ;;   - dividend is the concatenation
                      ;;     of edx and eax
                      ;;   - divisor in any reg (here ecx)
                      ;; 
                      ;; Since we only want 32-bit division,
                      ;; we fill edx with the sign of eax.
                      ;; 
                      ;; Quotient will be in eax, remainder
                      ;; in edx.
                      ((/ %)
                       ;; Get our divisor in place.
                       (isn "mov ecx, edx")
                       
                       ;; Fill edx with appropriate sign bits.
                       (isn "cdq")
                       
                       ;; Perform division.
                       (isn "idiv ecx")
                       
                       ;; Move remainder into eax if we wanted that.
                       (when (eq (car expr) '%)
                         (isn "mov eax, edx"))))
                    
                    ;; Push the result back onto the stack.
                    (isn "push eax")))
                 (t (error "unknown thing ~S" expr)))))
      
      ;; Generate all the code.
      (gen expr)
      
      ;; Get result into eax.
      (isn "pop eax")
      
      ;; Return instructions.
      (nreverse isns))))

;;; Example
;; CL-USER> (generate-asm '(+ (* 9 (/ 3 2)) (- 10 (% 5 3))))
;; ("push 9" "push 3" "push 2" "pop edx" "pop eax" "mov ecx, edx" "cdq" "idiv ecx"
;;  "push eax" "pop edx" "pop eax" "imul eax, edx" "push eax" "push 10" "push 5"
;;  "push 3" "pop edx" "pop eax" "mov ecx, edx" "cdq" "idiv ecx" "mov eax, edx"
;;  "push eax" "pop edx" "pop eax" "sub eax, edx" "push eax" "pop edx" "pop eax"
;;  "add eax, edx" "push eax" "pop eax")
