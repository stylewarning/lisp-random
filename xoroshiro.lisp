;;;; xoroshiro.lisp
;;;;
;;;; Copyright (c) 2018 Robert Smith

;;;; Implementation of xoroshiro128plus from
;;;;
;;;;     http://vigna.di.unimi.it/xorshift/xoroshiro128plus.c

(defpackage #:xoroshiro
  (:use #:cl)
  (:export
   #:next
   #:random-ub64
   #:random-ub32
   #:random-sb32
   #:random-single-float
   #:random-double-float))

(in-package #:xoroshiro)

(require :sb-rotate-byte)

(deftype ub32 ()
  '(unsigned-byte 32))

(deftype ub64 ()
  '(unsigned-byte 64))

(declaim (ftype (function (ub64 (integer 0 (64))) ub64) rotl)
         (inline rotl))
(defun rotl (x k)
  (sb-rotate-byte:rotate-byte k (byte 64 0) x))

(declaim (ftype (function (ub64 (integer 0 64)) ub64) <<)
         (inline <<))
(defun << (x k)
  (ldb (byte 64 0) (ash x k)))

(declaim (type (simple-array ub64 (2)) **random-state**))
(sb-ext:defglobal **random-state**
    (make-array 2 :element-type 'ub64
                  :initial-contents (list (random most-positive-fixnum)
                                          (random most-positive-fixnum))))

(declaim (ftype (function () ub64) next)
         (inline next))
(defun next ()
  (declare (optimize speed (safety 0) (debug 0) (space 0) (compilation-speed 0)))
  (let ((s **random-state**))
    (let* ((s0 (aref s 0))
           (s1 (aref s 1))
           (result (mod (+ s0 s1) #.(expt 2 64))))
      (declare (type ub64 s0 s1 result))
      (setf s1 (logxor s1 s0))
      (setf (aref s 0) (logxor (rotl s0 24)
                              s1
                              (<< s1 16)))
      (setf (aref s 1) (rotl s1 37))
      result)))

(declaim (ftype (function () ub64) random-ub64))
(defun random-ub64 ()
  (declare (optimize speed (safety 0) (debug 0) (space 0) (compilation-speed 0)))
  (next))

(declaim (ftype (function () ub32) random-ub32))
(defun random-ub32 ()
  (declare (optimize speed (safety 0) (debug 0) (space 0) (compilation-speed 0)))
  ;; Specifically take the high-order bits.
  (ldb (byte 32 32) (next)))

(declaim (ftype (function () (signed-byte 32)) random-sb32))
(defun random-sb32 ()
  (declare (optimize speed (safety 0) (debug 0) (space 0) (compilation-speed 0)))
  ;; Specifically take the high-order bits.
  (let* ((ub32 (ldb (byte 32 32) (next)))
         (sign (- (* 2 (logand 1 ub32)) 1)))
    (* sign (ash ub32 -1))))


(declaim (ftype (function () (single-float 0.0f0 (1.0f0))) random-single-float))
(defun random-single-float ()
  (declare (optimize speed (safety 0) (debug 0) (space 0) (compilation-speed 0)))
  (let ((mantissa (ldb (byte 23 #.(- 64 23)) (next))))
    (declare (type (unsigned-byte 23) mantissa))
    (let ((float-bits (logior mantissa
                              #.(ash 127 23) ; exponent
                              #.(ash 0 31)   ; sign
                              )))
      (declare (type ub32 float-bits))
      (1- (sb-kernel:make-single-float float-bits)))))

(declaim (ftype (function () (double-float 0.0d0 (1.0d0))) random-double-float))
(defun random-double-float ()
  (declare (optimize speed (safety 0) (debug 0) (space 0) (compilation-speed 0)))
  (let ((mantissa (ldb (byte 52 #.(- 64 52)) (next))))
    (declare (type (unsigned-byte 52) mantissa))
    (let ((float-bits (logior mantissa
                              #.(ash 1023 52) ; exponent
                              #.(ash 0 63)    ; sign
                              )))
      (declare (type ub64 float-bits))
      (1- (sb-kernel:make-double-float (ldb (byte 32 32) float-bits)
                                       (ldb (byte 32 0)  float-bits))))))

