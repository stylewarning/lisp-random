;;;; rdrand.lisp
;;;; Copyright (c) 2012 Robert Smith

;;;; Support for Ivy Bridge's RDRAND instruction.

(in-package "SB-VM")

;;; Define the instruction formats.

(sb!disassem:define-instruction-format (rdrand-format 24
                                        :default-printer '(:name :tab reg))
  ;; two byte operand
  (op     :field  (byte 16 0))
  
  ;; ModR/M format
  (mod    :field (byte 2 16) :value #b11)
  (opc    :field (byte 3 18) :value #b110)
  (reg    :field (byte 3 21)  :type 'reg-b))

(sb!disassem:define-instruction-format (ext-rdrand-format 24
                                        :default-printer '(:name :tab reg))
  ;; REX
  (prefix :field (byte 8 0))
  
  ;; two byte operand
  (op     :field  (byte 16 8))
  
  ;; ModR/M format
  (mod    :field (byte 2 24) :value #b11)
  (opc    :field (byte 3 26) :value #b110)
  (reg    :field (byte 3 8)  :type 'reg-b))


;;; The RDRAND instruction has the following layout:
;;; 
;;;    32-bit: #x0FC7 /6
;;; 
;;;    64-bit: REX.W + #x0FC7 /6

(define-instruction rdrand (segment dst)
  (:printer rdrand-format ((op #x0FC7)))             ; not quite right...
  (:emitter
   ;; Emit REX depending on register kind.
   ;; (maybe-emit-rex-for-ea segment dst nil)
   
   ;; Emit the RDRAND instruction bits.
   (emit-byte segment #x0F)
   (emit-byte segment #xC7)

   ;; Emit register code.
   (emit-mod-reg-r/m-byte segment
                          #b11
                          #b110
                          (reg-tn-encoding dst))))


;;; Tell the compiler about %RDRAND

(defknown %rdrand () fixnum)

;;; Define the assembly

(define-vop (rdrand)
  (:translate %rdrand)
  (:results (res :scs (any-reg)))
  (:result-types tagged-num)
  (:policy :fast-safe)
  (:generator 3
    RETRY-RDRAND
    (inst rdrand res)
    (inst jmp :nc RETRY-RDRAND)
    (inst shr res sb-vm:n-fixnum-tag-bits)))

#+#:ignore
(defun rdrand ()
  (%rdrand))

