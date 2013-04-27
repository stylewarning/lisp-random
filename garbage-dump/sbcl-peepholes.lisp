(declaim (optimize speed (safety 0) (debug 0)))

(deftype u32 () '(unsigned-byte 32))
(deftype u32* (size) `(simple-array u32 (,size)))

(deftype u64 () '(unsigned-byte 64))
(deftype u64* (size) `(simple-array u64 (,size)))


(defconstant +max32+ #xFFFFFFFF)
(defconstant +max64+ #xFFFFFFFFFFFFFFFF)

(declaim (ftype (function (u32 u32) u32) u32+)
         (inline u32+))
(defun u32+ (a b)
  (logand (+ a b) +max32+))

(declaim (ftype (function (u64 u64) u64) u64+))
(defun u64+ (a b)
  (logand (+ a b) +max64+))

(declaim (ftype (function (u32 u32) u32) max32)
         (inline max32))
(defun max32 (a b)
  (if (> a b) a b))

(declaim (ftype (function (u32 u32 u32) u32) max32-3))
(defun max32-3 (a b c)
  (max32 (max32 a b) c))

(declaim (ftype (function ((u32* 16)) u32) max32-16))
(defun max32-16 (arr)
  (let ((found 0))
    (dotimes (idx 16 found)
      (setf found (max32 found (aref arr idx))))))
;       37:       4C8BC1           MOV R8, RCX
;       3A:       488BFB           MOV RDI, RBX
;       3D:       4839D9           CMP RCX, RBX
;       40:       488BCF           MOV RCX, RDI
;       43:       490F4FC8         CMOVNLE RCX, R8
; there's no point putting rbx into rdi
; it can just be rcx = rbx

(declaim (ftype (function ((u32* 16)) u32) sum32-16))
(defun sum32-16 (arr)
  (let ((sum 0))
    (dotimes (idx 16 sum)
      (setf sum (u32+ sum (aref arr idx))))))


(declaim (ftype (function ((u32* 16)) (u32* 16)) rev32-16))
(defun rev32-16 (arr)
  (dotimes (idx 8 arr)
    (rotatef (aref arr (- 15 idx)) 
             (aref arr idx))))

(declaim (ftype (function ((u32* 16)) (u32* 16)) zero32-16))
(defun zero32-16 (arr)
  ;; This could be improved
  #+ignore
  (map-into arr (constantly 0) arr)
  (dotimes (i 16 arr)
    (setf (aref arr i) 0)))
;;      5F0: L0:   31C0             XOR EAX, EAX
;;      5F2:       89444A01         MOV [RDX+RCX*2+1], EAX
;; -->
;;      MOV [...], 0
;;
;; or move XOR EAX EAX outside of the loop
;;
;; also...
;;
;;      5F6:       4883C102         ADD RCX, 2
;;      5FA: L1:   4883F920         CMP RCX, 32
;;      5FE:       7CF0             JL L0
;; -->
;; so when you add/sub, that sets the flags, so if you can make it a
;; countdown, not a count up to 32, you can just SUB RCX, 2 : JNZ L0


(declaim (ftype (function ((u32* 16)) (u32* 16)) three32-16))
(defun three32-16 (arr)
  (dotimes (i 16 arr)
    (setf (aref arr i) #xFFFFFFFE)))

(declaim (ftype (function ((u64* 16)) (u64* 16)) three64-16))
(defun three64-16 (arr)
  (dotimes (i 16 arr)
    (setf (aref arr i) 15028999435905310454)))

;; super gr0ss
(declaim (ftype (function ((u32* 16)) (u32* 16)) rand32-16))
(defun rand32-16 (arr)
  (dotimes (i 16 arr)
    (setf (aref arr i) (random +max32+))))

(defun swap32-16 (a b)
  (declare (type (u32* 16) a b))
  (dotimes (i 16)
    (rotatef (aref a i) (aref b i))))

(defun swaprev32-16 (a b)
  (declare (type (u32* 16) a b))
  (dotimes (i 16)
    (rotatef (aref a i) (aref b (- 15 i)))))

(declaim (ftype (function ((u32* 256)) (u32* 256)) square32-256))
(defun square32-256 (arr)
  (dotimes (i 16 arr)
    (let ((i (* 16 i)))
      (dotimes (j 16)
        (let ((idx (+ j i)))
          (setf (aref arr idx) idx))))))

(declaim (ftype (function ((u32* 256)) (u32* 256)) square32-256-linear))
(defun square32-256-linear (arr)
  (dotimes (i 256 arr)
    (setf (aref arr i) i)))



(declaim (ftype (function (u64 u64) u64) max64))
(defun max64 (a b)
  (if (> a b) a b))
