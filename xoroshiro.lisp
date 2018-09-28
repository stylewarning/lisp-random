;;;; xoroshiro.lisp
;;;;
;;;; Copyright (c) 2018 Robert Smith

;;;; A faster replacement of Lisp's RANDOM.
;;;;
;;;; Implementation of xoroshiro128plus from
;;;;
;;;;     http://vigna.di.unimi.it/xorshift/xoroshiro128plus.c

(require :sb-rotate-byte)

(defpackage #:xoroshiro
  (:use #:cl)
  (:shadow #:random-state
           #:make-random-state
           #:*random-state*)
  (:export
   #:random-state
   #:make-random-state
   #:*random-state*
   #:seed-random-state
   #:next
   #:random-ub64
   #:random-ub32
   #:random-sb32
   #:random-single-float
   #:random-double-float))

(in-package #:xoroshiro)

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

(deftype random-state ()
  "A Xoroshiro128+ random state."
  '(simple-array ub64 (2)))

(defun make-raw-random-state (a b)
  (declare (type ub64 a b))
  (let ((new-state (make-array 2 :element-type 'ub64)))
    ;; Avoid a baby cons of (LIST A B). :-)
    (setf (aref new-state 0) a
          (aref new-state 1) b)
    new-state))

(declaim (special *random-state*))      ; forward declaration
(declaim (type random-state *random-state*))

(defun make-random-state (&optional state)
  "Create a fresh Xoroshiro128+ random state according to the same rules of CL:MAKE-RANDOM-STATE."
  (etypecase state
    (random-state (copy-seq state))
    (null         (copy-seq *random-state*))
    ((member t)   (make-raw-random-state (cl:random #.(expt 2 64))
                                         (cl:random #.(expt 2 64))))))

(defvar *random-state* (make-random-state t)
  "The random state used in the Xoroshiro128+ pseudorandom number generator.")

(defun seed-random-state (state)
  "Seed *RANDOM-STATE* with either a RANDOM-STATE object or a 128-bit unsigned integer."
  (etypecase state
    (random-state
     (setf *random-state* state))
    ((unsigned-byte 128)
     (setf *random-state*
           (make-raw-random-state (ldb (byte 64  0) state)
                                  (ldb (byte 64 64) state))))))

(declaim (ftype (function () ub64) next)
         (inline next))
(defun next ()
  (declare (optimize speed (safety 0) (debug 0) (space 0) (compilation-speed 0)))
  (let ((s *random-state*))
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

(declaim (ftype (function () ub64) random-ub64)
         (inline random-ub64))
(defun random-ub64 ()
  (declare (optimize speed (safety 0) (debug 0) (space 0) (compilation-speed 0)))
  (next))
(declaim (notinline random-ub64))

(declaim (ftype (function () ub32) random-ub32)
         (inline random-ub32))
(defun random-ub32 ()
  (declare (optimize speed (safety 0) (debug 0) (space 0) (compilation-speed 0)))
  ;; Specifically take the high-order bits.
  (ldb (byte 32 32) (next)))
(declaim (notinline random-u32))

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


#+sbcl
(defun bench (n)
  (declare (optimize speed (safety 0) (debug 0) (space 0) (compilation-speed 0))
           (type (and fixnum unsigned-byte) n)
           (inline random-ub64))
  (sb-ext:gc :full t)
  (let ((ms 0))
    (setf ms (get-internal-real-time))
    (loop :repeat n
          :for r :of-type ub64 := (random-ub64)
          :count (evenp r))
    (format t "~&XOROSHIRO: ~D calls/ms~%"
            (round
             (/ n
                (/ (* 1000 (- (get-internal-real-time) ms))
                   internal-time-units-per-second))))
    (sb-ext:gc :full t)
    (setf ms (get-internal-real-time))
    (loop :repeat n
          :for r :of-type ub64 := (random #.(expt 2 64))
          :count (evenp r))
    (format t "~&CL:RANDOM: ~D calls/ms~%"
            (round
             (/ n
                (/ (* 1000 (- (get-internal-real-time) ms))
                   internal-time-units-per-second)))))
  t)
