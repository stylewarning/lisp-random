;;;; sb-sincos.lisp
;;;;
;;;; Author: Robert Smith, 2015

(defpackage #:sb-sincos
  (:use #:cl #:sb-c)
  (:export #:sincos))

(in-package #:sb-sincos)

;;; The "sincos" function is often provided by C math libraries and
;;; allows the computation of both sine and cosine simultaneously.
;;;
;;; GNU libm has the following functions:
;;;
;;;    sincos(double x, double *sin, double *cos)
;;;    sincosf(float x, float *sin, float *cos)
;;;    sincosl(long double x, long double *sin, long double *cos)
;;;
;;; We only use the former two.
;;;
;;; Apple OS X has similar functions:
;;;
;;;    __sincos(double x, double *sin, double *cos)
;;;    __sincosf(float x, float *sin, float *cos)
;;;
;;; It is noted within /usr/include/math.h next to these functions
;;; that they've been available starting with Mac OS 10.9. Also of
;;; note, Apple used GCC pre-10.7.

(macrolet ((define-sincos (name c-name type)
             `(progn
                (defun ,name (x)
                  (sb-alien:with-alien ((sin ,type)
                                        (cos ,type))
                    (sb-alien:alien-funcall
                     (sb-alien:extern-alien ,c-name (function sb-alien:void
                                                              ,type
                                                              (* ,type)
                                                              (* ,type)))
                     x
                     (sb-alien:addr sin)
                     (sb-alien:addr cos))
                    (values sin cos))))))
  (declaim (inline %sincos-single %sincos-double)
           (ftype (function (single-float) (values single-float single-float))
                  %sincos-single)
           (ftype (function (double-float) (values double-float double-float))
                  %sincos-double))
  #+darwin
  (progn
    (define-sincos %sincos-single "__sincosf" single-float)
    (define-sincos %sincos-double "__sincos" double-float))
  #+linux
  (progn
    (define-sincos %sincos-single "sincosf" single-float)
    (define-sincos %sincos-double "sincos" double-float))
  #-(or darwin linux)
  (progn
    (defun %sincos-single (x)
      (declare (type single-float x))
      (values (sin x) (cos x)))
    (defun %sincos-double (x)
      (declare (type double-float x))
      (values (sin x) (cos x)))))

(declaim (ftype (function (number) (values (or (float -1.0 1.0) (complex float))
                                           (or (float -1.0 1.0) (complex float))
                                           &optional))
                sincos)
         ;; We decide to universally inline it since it boils down to
         ;; either an alien call or two trig calls.
         (inline sincos))
(defun sincos (number)
  "Return two values: the sine and cosine of NUMBER.

This function may be more efficient than individually calling SIN and COS."
  (etypecase number
    (single-float (%sincos-single number))
    (double-float (%sincos-double number))
    (number (values (sin number) (cos number)))))

(defknown sincos (number)
  (values (or (float -1.0 1.0) (complex float))
          (or (float -1.0 1.0) (complex float)))
    (movable foldable flushable sb-c::explicit-check sb-c::recursive)
  :derive-type (lambda (call)
                 (let ((sv (sb-c::result-type-float-contagion call)))
                   (sb-c::make-values-type :required (list sv sv)))))




;;;;;;;;;;;;;;;;;;;;;;;;; Benchmarks & Tests ;;;;;;;;;;;;;;;;;;;;;;;;;

;;; For analyzing disassemblies

(defun sincos-single (x)
  (declare (optimize speed (safety 0) (debug 0) (space 0))
           (type single-float x))
  (multiple-value-bind (sin cos)
      (sincos x)
    (+ sin cos)))

(defun sincos-double (x)
  (declare (optimize speed (safety 0) (debug 0) (space 0))
           (type double-float x))
  (multiple-value-bind (sin cos)
      (sincos x)
    (+ sin cos)))

(defun sincos-complex (x)
  (declare (optimize speed (safety 0) (debug 0) (space 0))
           (type complex x))
  (multiple-value-bind (sin cos)
      (sincos x)
    (+ sin cos)))

(defun time-it (fn iters)
  (sb-ext:gc :full t)
  (loop :with start := (get-internal-real-time)
        :repeat iters
        :do (funcall fn)
        :finally (return (* 1000 (/ (- (get-internal-real-time) start)
                                    internal-time-units-per-second)))))


;;; Sample results:
;;;
;;; Column 1: # of iterations
;;; Column 2: Percentage speed-up for single floats.
;;; Column 3: Percentage speed-up for double floats.
;;; 
;;;     1 50.0      50.0
;;;    10 50.0      30.0
;;;   100 47.027027 14.444445
;;;  1000 48.904327 14.125957
;;; 10000 48.34637  13.835488
;;;
(defun bench ()
  (labels ((bench-single-naive ()
             (declare (optimize speed (safety 0) (debug 0) (space 0)))
             (loop :with s :of-type single-float := 0.0
                   :for angle :of-type single-float :from 0.0 :below 6.282 :by 0.0001
                   :do (let ((sin (sin angle))
                             (cos (cos angle)))
                         (incf s (+ sin cos)))))
           (bench-single-sincos ()
             (declare (optimize speed (safety 0) (debug 0) (space 0)))
             (loop :with s :of-type single-float := 0.0
                   :for angle :of-type single-float :from 0.0 :below 6.282 :by 0.0001
                   :do (incf s (multiple-value-call #'+ (sincos angle)))))
           (bench-double-naive ()
             (declare (optimize speed (safety 0) (debug 0) (space 0)))
             (loop :with s :of-type double-float := 0.0d0
                   :for angle :of-type double-float :from 0.0d0 :below 6.282d0 :by 0.0001d0
                   :do (let ((sin (sin angle))
                             (cos (cos angle)))
                         (incf s (+ sin cos)))))
           (bench-double-sincos ()
             (declare (optimize speed (safety 0) (debug 0) (space 0)))
             (loop :with s :of-type double-float := 0.0d0
                   :for angle :of-type double-float :from 0.0d0 :below 6.282d0 :by 0.0001d0
                   :do (incf s (multiple-value-call #'+ (sincos angle)))))
           (bench-for (iters)
             (let* ((sn (time-it #'bench-single-naive iters))
                    (ssc (time-it #'bench-single-sincos iters))
                    (dn (time-it #'bench-double-naive iters))
                    (dsc (time-it #'bench-double-sincos iters))
                    (single-speedup (float (* 100 (/ (- sn ssc) sn))))
                    (double-speedup (float (* 100 (/ (- dn dsc) dn)))))
               (list single-speedup double-speedup))))
    (loop :for power :to 4
          :for iters := (expt 10 power)
          :collect (cons iters (bench-for iters)))))


