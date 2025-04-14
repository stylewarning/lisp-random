;;;; Code accompanying my ELS 2025 talk

(cl:defpackage #:fft
  (:use #:cl)
  (:export #:test))

(cl:in-package #:fft)

(defun test (fn)
  (let* ((n (expt 2 20))
         (x (make-array n :element-type '(complex double-float)
                          :initial-element #C(0.0d0 0.0d0))))
    (loop :for i :below n
          :do (setf (aref x i) (coerce (sin (/ (* 2 pi i) n)) '(complex double-float))))
    (time (funcall fn x))
    nil))

;;; PLAIN OLD LISP CODE
;;;
;;; Let's start by writing Lisp code in the usual Lisp style. We want
;;; to do this as directly as possible, without regard to performance
;;; or anything like that.

(cl:defpackage #:fft/cl1
  (:use #:cl)
  (:export #:fft))

(cl:in-package #:fft/cl1)

(defun fft-root (x n w)
  (loop :for l := 2 :then (* 2 l)
        :while (<= l n)
        :for l/2 := (/ l 2)
        :do (loop :for j :below l/2
                  :for w^j := 1 :then (* w w^j)
                  :do (dotimes (k (/ n l))
                        (let* ((kl+j (+ j (* k l)))
                               (tao (* w^j (aref x (+ kl+j l/2)))))
                          (setf (aref x (+ kl+j l/2)) (+ (aref x kl+j) tao))
                          (setf (aref x kl+j)         (- (aref x kl+j) tao)))))
        :finally (return x)))

(defun root (n)
  (cis (/ (* -2 pi) n)))

(defun fft (x)
  (let* ((n (length x))
         (w (root n)))
    (fft-root x n w)))

(cl:defpackage #:fft/coal1
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:arr  #:COALTON-LIBRARY/LISPARRAY)
   (#:loop #:coalton-library/experimental/loops)
   (#:math #:COALTON-LIBRARY/MATH))
  (:export #:fft))

(cl:in-package #:fft/coal1)

(coalton-toplevel
  (define (fft-root x n w)
    (rec decimate ((l 2))
      (cond
        ((> l n)
         x)
        (True
         (let ((l/2 (math:div l 2)))
           (rec roots ((j 0) (w^j 1))
             (when (< j l/2)
               (loop:dotimes (k (math:div n l))
                 (let ((kl+j (+ j (* k l)))
                       (tao (* w^j (arr:aref x (+ kl+j l/2)))))
                   (arr:set! x (+ kl+j l/2) (+ (arr:aref x kl+j) tao))
                   (arr:set! x kl+j (- (arr:aref x kl+j) tao))))
               (roots (+ j 1) (* w w^j)))))
         (decimate (* 2 l))))))

  (define (root n)
    (math:cis (/ (* -2 math:pi)
                 (unwrap (tryinto n)))))

  ;; Tell Lisp the actual argument types so that we can call the
  ;; function from Lisp.
  (declare fft (arr:LispArray (Complex Double-Float) -> arr:LispArray (Complex Double-Float)))
  (define (fft x)
    (let ((n (arr:length x))
          (w (root n)))
      (fft-root x n w))))

;;; FUNCTION TYPE DECLARATIONS
;;;
;;; We see that both implementations are equally slow. The main reason
;;; is that both implementations are relatively generic. For instance,
;;; the Lisp code leans on its numerical tower and will coerce types
;;; as needed. The Lisp code also works on any vector.
;;;
;;; One way to improve performance is to label each function's
;;; type. Some Lisp implementations don't do much with this
;;; information, but SBCL is specifically quite good and aggressive
;;; with this information.

(cl:defpackage #:fft/cl2
  (:use #:cl)
  (:export #:fft))

(cl:in-package #:fft/cl2)

(declaim (ftype (function ((simple-array (complex double-float) (*))
                           fixnum
                           (complex double-float))
                          (simple-array (complex double-float) (*)))
                fft-root))
(defun fft-root (x n w)
  (loop :for l := 2 :then (* 2 l)
        :while (<= l n)
        :for l/2 := (/ l 2)
        :do (loop :for j :below l/2
                  :for w^j := 1 :then (* w w^j)
                  :do (dotimes (k (/ n l))
                        (let* ((kl+j (+ j (* k l)))
                               (tao (* w^j (aref x (+ kl+j l/2)))))
                          (setf (aref x (+ kl+j l/2)) (+ (aref x kl+j) tao))
                          (setf (aref x kl+j)         (- (aref x kl+j) tao)))))
        :finally (return x)))

(declaim (ftype (function (fixnum) (complex double-float)) root))
(defun root (n)
  (cis (/ (* -2 pi) n)))

(declaim (ftype (function ((simple-array (complex double-float) (*)))
                          (simple-array (complex double-float) (*)))
                fft))
(defun fft (x)
  (let* ((n (length x))
         (w (root n)))
    (fft-root x n w)))

(cl:defpackage #:fft/coal2
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:arr  #:COALTON-LIBRARY/LISPARRAY)
   (#:loop #:coalton-library/experimental/loops)
   (#:math #:COALTON-LIBRARY/MATH))
  (:export #:fft))

(cl:in-package #:fft/coal2)

(coalton-toplevel
  (declare fft-root ((arr:LispArray (Complex Double-Float))
                     -> UFix
                     -> (Complex Double-Float)
                     -> (arr:LispArray (Complex Double-Float))))
  (define (fft-root x n w)
    (rec decimate ((l 2))
      (cond
        ((> l n)
         x)
        (True
         (let ((l/2 (math:div l 2)))
           (rec roots ((j 0) (w^j 1))
             (when (< j l/2)
               (loop:dotimes (k (math:div n l))
                 (let ((kl+j (+ j (* k l)))
                       (tao (* w^j (arr:aref x (+ kl+j l/2)))))
                   (arr:set! x (+ kl+j l/2) (+ (arr:aref x kl+j) tao))
                   (arr:set! x kl+j (- (arr:aref x kl+j) tao))))
               (roots (+ j 1) (* w w^j)))))
         (decimate (* 2 l))))))

  (declare root (UFix -> (Complex Double-Float)))
  (define (root n)
    (math:cis (/ (* -2 math:pi)
                 (unwrap (tryinto n)))))

  (declare fft ((arr:LispArray (Complex Double-Float))
                -> (arr:LispArray (Complex Double-Float))))
  (define (fft x)
    (let ((n (arr:length x))
          (w (root n)))
      (fft-root x n w))))

;;; From Lisp 1 to Lisp 2, we got about a 2x improvement in both time
;;; and memory.
;;;
;;; Lisp 2 to Coalton 2 is nearly 15x faster and eliminated all
;;; consing.
;;;
;;; Now, we know we can do as well in Lisp, so we have to roll up our
;;; sleeves and add more declarations.

(cl:defpackage #:fft/cl2.5
  (:use #:cl)
  (:export #:fft))

(cl:in-package #:fft/cl2.5)

(declaim (ftype (function ((simple-array (complex double-float) (*))
                           fixnum
                           (complex double-float))
                          (simple-array (complex double-float) (*)))
                fft-root))
(defun fft-root (x n w)
  (loop :for l :of-type fixnum := 2 :then (* 2 l)
        :while (<= l n)
        :for l/2 :of-type fixnum := (/ l 2)
        :do (loop :for j :of-type fixnum :below l/2
                  :for w^j :of-type (complex double-float) := #C(1.0d0 0.0d0) :then (* w w^j)
                  :do (dotimes (k (the fixnum (/ n l)))
                        (declare (type fixnum k))
                        (let* ((kl+j (+ j (* k l)))
                               (tao (* w^j (aref x (the fixnum (+ kl+j l/2))))))
                          (declare (type fixnum kl+j)
                                   (type (complex double-float) tao))
                          (setf (aref x (+ kl+j l/2)) (+ (aref x kl+j) tao))
                          (setf (aref x kl+j)         (- (aref x kl+j) tao)))))
        :finally (return x)))

(declaim (ftype (function (fixnum) (complex double-float)) root))
(defun root (n)
  (cis (/ (* -2 pi) n)))

(declaim (ftype (function ((simple-array (complex double-float) (*)))
                          (simple-array (complex double-float) (*)))
                fft))
(defun fft (x)
  (let* ((n (length x))
         (w (root n)))
    (fft-root x n w)))


;;; And now we are at performance parity.
;;;
;;; Now, the code did have to get uglier. We needed to add lots of
;;; type declarations. Maybe more than we needed because SBCL is
;;; reasonably good at propagating types, but they were still
;;; necessary. We also needed to change some of our constants, like
;;; "1" (which is an INTEGER) to "#C(1.0d0 0.0d0)" (which is a
;;; (COMPLEX DOUBLE-FLOAT)).
;;;
;;; Coalton is appreciably cleaner. The toplevel type declarations
;;; were enough.
;;;
;;; Now I want to go in a different direction. What if we wanted to
;;; support other types? Maybe this is an FFT library, and we don't
;;; want to assume that the user is using DOUBLE-FLOATs. Maybe they're
;;; using SINGLE-FLOATs, or even finite fields.
;;;
;;; Let's write a more generic version of each that supports at least
;;; the following three types:
;;;
;;;     (COMPLEX SINGLE-FLOAT)
;;;     (COMPLEX DOUBLE-FLOAT)
;;;     Integers mod 7340033 = 1 + 2^20 * 7  (prim root 2187)
;;;
;;; Here, we want to make it so that the user could extend with
;;; additional types if they wanted. As such, we need a protocol for
;;; extension.
(cl:defpackage #:fft/cl3
  (:use #:cl)
  (:export #:fft-sf #:fft-df))

(cl:in-package #:fft/cl3)

(defgeneric one (type))
(defgeneric plus (type a b))
(defgeneric minus (type a b))
(defgeneric times (type a b))
(defgeneric root (type n))
(defgeneric inv-root (type n))

(defun fft-root (type x n w)
  (loop :for l := 2 :then (* 2 l)
        :while (<= l n)
        :for l/2 := (/ l 2)
        :do (loop :for j :below l/2
                  :for w^j := (one type) :then (times type w w^j)
                  :do (dotimes (k (/ n l))
                        (let* ((kl+j (+ j (* k l)))
                               (tao (times type w^j (aref x (+ kl+j l/2)))))
                          (setf (aref x (+ kl+j l/2)) (plus  type (aref x kl+j) tao))
                          (setf (aref x kl+j)         (minus type (aref x kl+j) tao)))))
        :finally (return x)))

(defun fft (type x)
  (let* ((n (length x))
         (w (root type n)))
    (fft-root type x n w)))


;;; SINGLE-FLOAT: SF
(defmethod one ((x (eql 'sf)))        #C(1.0f0 0.0f0))
(defmethod plus ((x (eql 'sf)) a b)   (+ a b))
(defmethod minus ((x (eql 'sf)) a b)  (- a b))
(defmethod times ((x (eql 'sf)) a b)  (* a b))
(defmethod root ((x (eql 'sf)) n)     (cis (/ (* -2 (coerce pi 'single-float)) n)))
(defun fft-sf (x)                     (fft 'sf x))

;;; DOUBLE-FLOAT: DF
(defmethod one ((x (eql 'df)))        #C(1.0d0 0.0d0))
(defmethod plus ((x (eql 'df)) a b)   (+ a b))
(defmethod minus ((x (eql 'df)) a b)  (- a b))
(defmethod times ((x (eql 'df)) a b)  (* a b))
(defmethod root ((x (eql 'df)) n)     (cis (/ (* -2 pi) n)))
(defun fft-df (x)                     (fft 'df x))

;;; FINITE FIELD: FF
(defmethod one ((x (eql 'ff)))        1)
(defmethod plus ((x (eql 'ff)) a b)   (mod (+ a b) 7340033))
(defmethod minus ((x (eql 'ff)) a b)  (mod (- a b) 7340033))
(defmethod times ((x (eql 'ff)) a b)  (mod (* a b) 7340033))
(defmethod root ((x (eql 'ff)) n)     (mod (expt 4665133 n) 7340033))
(defun fft-ff (x)                     (fft 'ff x))



(cl:defpackage #:fft/coal3
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:arr  #:COALTON-LIBRARY/LISPARRAY)
   (#:loop #:coalton-library/experimental/loops)
   (#:math #:COALTON-LIBRARY/MATH))
  (:export #:fft-sf #:fft-df #:fft-df*))

(cl:in-package #:fft/coal3)



(coalton-toplevel
  (define-class (PrimitiveRoot :t)
    (root (UFix -> :t)))

  (define (fft-root x n w)
    (rec decimate ((l 2))
      (cond
        ((> l n)
         x)
        (True
         (let ((l/2 (math:div l 2)))
           (rec roots ((j 0) (w^j 1))
             (when (< j l/2)
               (loop:dotimes (k (math:div n l))
                 (let ((kl+j (+ j (* k l)))
                       (tao (* w^j (arr:aref x (+ kl+j l/2)))))
                   (arr:set! x (+ kl+j l/2) (+ (arr:aref x kl+j) tao))
                   (arr:set! x kl+j (- (arr:aref x kl+j) tao))))
               (roots (+ j 1) (* w w^j)))))
         (decimate (* 2 l))))))

  (define (fft x)
    (let ((n (arr:length x))
          (w (root n)))
      (fft-root x n w)))

  (define-instance (PrimitiveRoot (Complex Single-Float))
    (define (root n)
      (math:cis (/ (* -2 math:pi)
                   (unwrap (tryinto n))))))

  (define-instance (PrimitiveRoot (Complex Double-Float))
    (define (root n)
      (math:cis (/ (* -2 math:pi)
                   (unwrap (tryinto n))))))
  
  (declare fft-sf (arr:LispArray (Complex Single-Float) -> arr:LispArray (Complex Single-Float)))
  (define (fft-sf x)
    (fft x))

  (declare fft-df (arr:LispArray (Complex Double-Float) -> arr:LispArray (Complex Double-Float)))
  (define (fft-df x)
    (fft x))

  (monomorphize)
  (declare fft-df* (arr:LispArray (Complex Double-Float) -> arr:LispArray (Complex Double-Float)))
  (define (fft-df* x)
    (fft x)))

(coalton-toplevel
  (repr :transparent)
  (define-type FiniteField (F UFix))

  (define-instance (Eq FiniteField)
    (define (== (F a) (F b))
      (== a b)))

  (define-instance (Num FiniteField)
    (define (+ (F a) (F b))
      (F (lisp UFix (a b) (cl:mod (cl:+ a b) 7340033))))
    (define (- (F a) (F b))
      (F (lisp UFix (a b) (cl:mod (cl:- a b) 7340033))))
    (define (* (F a) (F b))
      (F (lisp UFix (a b) (cl:mod (cl:* a b) 7340033))))
    (define (fromInt x)
      (F (lisp UFix (x) (cl:mod x 7340033)))))

  (define-instance (PrimitiveRoot FiniteField)
    (define (root k)
      (F (^ 4665133 k))))

  (monomorphize)
  (declare fft-ff (arr:LispArray FiniteField -> arr:LispArray FiniteField))
  (define (fft-ff x)
    (fft x)))
