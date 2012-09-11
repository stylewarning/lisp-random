;;;; rdrand.lisp
;;;; Copyright (c) 2012 Robert Smith

;;;; Support for Ivy Bridge's RDRAND instruction.

(in-package "SB-VM")

;;; The RDRAND instruction has the following layout:
;;; 
;;;    32-bit: #x0FC7
;;; 
;;;    64-bit: REX + #x0FC7

(define-instruction rdrand (segment dst)
  ;;(:printer reg/mem ((op #x0FC7)))
  (:emitter
   ;; Emit REX depending on register kind.
   (maybe-emit-rex-for-ea segment dst nil)
   
   ;; Emit the RDRAND instruction bits.
   (emit-byte segment #x0FC7)
   
   ;; Emit register code.
   (emit-ea segment dst #b011)))


;;; Tell the compiler about %RDRAND

(defknown %rdrand () fixnum)

;;; Define the assembly

(define-vop (rdrand)
  (:translate %rdrand)
  (:results (res :scs (any-reg)))
  (:result-types tagged-num)
  (:policy :fast)
  (:generator 3
    RETRY-RDRAND
    (inst rdrand res)
    (inst jmp :nc RETRY-RDRAND)
    (inst sar res sb-vm:n-fixnum-tag-bits)))




