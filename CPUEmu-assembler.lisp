;;;; CPUEmu-assembler.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith
;;;;
;;;; An assembler for https://bitbucket.org/linuxlalala/cpuemu/src/430462db76150adb34066357e8072923a42564d6/main.c?at=master
;;;;
;;;; Simulator comes along free of charge.

(defun bytep (x)
  (and (integerp x)
       (<= 0 x 255)))

(defvar *instruction-table*
  ;; INSTR  OPCODE  ARITY
  '((NOP    #x00    0)
    (LDX    #x01    1)
    (LDY    #x02    1)
    (JMP    #x03    1)
    (STX    #x04    1)
    (STY    #x05    1)
    (SMX    #x06    1)
    (SMY    #x07    1)
    (BEX    #x08    2)
    (BNX    #x09    2)
    (ADX    #x0A    1)
    (ADY    #x0B    1)
    (ORX    #x0C    1)
    (ORY    #x0D    1)
    (ANX    #x0E    1)
    (ANY    #x0F    1)
    (XOX    #x10    1)
    (XOY    #x11    1)
    (NOX    #x12    0)
    (NOY    #x13    0)
    (LIX    #x14    0)
    (LIY    #x15    0)
    (HLT    #xFF    0)))

(defun lookup-instruction (instr)
  (let ((entry (assoc instr *instruction-table*)))
    (if (null entry)
        (error "Invalid instruction ~S" instr)
        entry)))

(defun instruction-opcode (instr)
  (let ((entry (lookup-instruction instr)))
    (second entry)))

(defun instruction-arity (instr)
  (let ((entry (lookup-instruction instr)))
    (third entry)))

(defun validate-instruction (full-instr)
  (assert (listp full-instr) nil
          "Instruction must be a list. Found ~S." full-instr)
  (let ((arity (instruction-arity (first full-instr)))
        (arguments (rest full-instr)))
    (assert (and (= arity (length arguments))
                 (every #'bytep arguments)))
    full-instr))

(defun assemble (instructions)
  (let ((bytes '()))
    (labels ((emit (byte)
               (push byte bytes))
             (assemble-instruction (full-instr)
               (let ((instr (validate-instruction full-instr)))
                 (emit (instruction-opcode (first instr)))
                 (mapc #'emit (rest instr)))))
      (mapc #'assemble-instruction instructions)
      (nreverse bytes))))

;;; What the hell, let's just write a simulator too.
;;;
;;; A machine has MEMORY, X, Y, and PC

(defstruct machine
  (pc 0)
  (x 0)
  (y 0)
  (mem (make-array 256 :initial-element 0)))

(defun advance (m &optional (amount 1))
  (incf (machine-pc m) amount))

(defun bad-opcode (i)
  (lambda (machine)
    (declare (ignore machine))
    (error "ya dun goofed ~X" i)))

(defvar *action-table*
  (make-array 256 :initial-contents (loop :for i :below 256
                                          :collect (bad-opcode i))))

(defun lookup-action (opcode)
  (if (not (bytep opcode))
      (error "invalid opcode ~S" opcode)
      (aref *action-table* opcode)))

(defmacro define-instruction-action (instr (machine &rest slots) &body body)
  (check-type machine symbol)
  (flet ((frob (slot)
           `(,slot ,slot)))
    (let ((opcode (instruction-opcode instr)))
      `(progn
         (setf (aref *action-table* ,opcode)
               (lambda (,machine)
                 (declare (ignorable ,machine))
                 (with-slots ,(mapcar #'frob slots)
                     ,machine
                   ,@body)))))))


(define-instruction-action NOP (m)
  (advance m))

(define-instruction-action LDX (m pc x mem)
  (setf x (aref mem (1+ pc)))
  (advance m 2))

(define-instruction-action LDY (m pc y mem)
  (setf y (aref mem (1+ pc)))
  (advance m 2))

(define-instruction-action JMP (m pc mem)
  (setf pc (aref mem (1+ pc))))

(define-instruction-action STX (m pc x mem)
  (setf (aref mem (1+ pc)) x)
  (advance m 2))

(define-instruction-action STY (m pc y mem)
  (setf (aref mem (1+ pc)) y)
  (advance m 2))

(define-instruction-action SMX (m pc x mem)
  (setf x (aref mem (1+ pc)))
  (advance m 2))

(define-instruction-action SMY (m pc y mem)
  (setf y (aref mem (1+ pc)))
  (advance m 2))

(define-instruction-action BEX (m pc mem x)
  (if (= x (aref mem (aref mem (1+ pc))))
      (setf pc (aref mem (+ pc 2)))
      (advance m 3)))

(define-instruction-action BNX (m pc mem x)
  (if (/= x (aref mem (aref mem (1+ pc))))
      (setf pc (aref mem (+ pc 2)))
      (advance m 3)))

(define-instruction-action ADX (m pc x mem)
  (incf x (aref mem (1+ pc)))
  (advance m 2))

(define-instruction-action ADY (m pc y mem)
  (incf y (aref mem (1+ pc)))
  (advance m 2))

(define-instruction-action ORX (m pc x mem)
  (setf x (logior x (aref mem (1+ pc))))
  (advance m 2))

(define-instruction-action ORY (m pc y mem)
  (setf y (logior y (aref mem (1+ pc))))
  (advance m 2))

(define-instruction-action ANX (m pc x mem)
  (setf x (logand x (aref mem (1+ pc))))
  (advance m 2))

(define-instruction-action ANY (m pc y mem)
  (setf y (logand y (aref mem (1+ pc))))
  (advance m 2))

(define-instruction-action XOX (m pc x mem)
  (setf x (logxor x (aref mem (1+ pc))))
  (advance m 2))

(define-instruction-action XOY (m pc y mem)
  (setf y (logxor y (aref mem (1+ pc))))
  (advance m 2))

(define-instruction-action NOX (m x)
  (setf x (ldb (byte 8 0) (lognot x)))
  (advance m))

(define-instruction-action NOY (m y)
  (setf y (ldb (byte 8 0) (lognot y)))
  (advance m))

(define-instruction-action LIX (m x y mem)
  (setf x (aref mem y))
  (advance m))

(define-instruction-action LIY (m x y mem)
  (setf y (aref mem x))
  (advance m))

(define-instruction-action HLT (m)
  (throw :halt nil))


(defun run (machine)
  (flet ((fetch ()
           (aref (machine-mem machine) (machine-pc machine))))
    (catch :halt
      (loop
        (funcall (lookup-action (fetch)) machine)))))

(defun assemble-to-machine (assembly)
  (let ((machine (make-machine))
        (code (assemble assembly)))
    (loop :for i :from 0
          :for word :in code
          :do (setf (aref (machine-mem machine) i) word))
    machine))
