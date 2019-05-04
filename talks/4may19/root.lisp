;;;; root.lisp
;;;;
;;;; Author: Robert Smith

;;; This file is intended to be a tutorial for using conditions and
;;; restarts in Common Lisp. It is based around computing the roots of
;;; a real function.
;;;
;;; It was presented at the Bay Area Julia Users meetup on 4 May 2019.

(defpackage #:root
  (:documentation "A package for finding roots of functions.")
  (:use #:cl)
  (:export
   #:divergence-error                   ; CONDITION
   #:divergence-error-method            ; READER
   #:divergence-error-iterations        ; READER

   #:bisect                             ; FUNCTION
   #:newton                             ; FUNCTION

   #:find-root                          ; FUNCTION
   #:try-newton                         ; RESTART
   #:try-bisection                      ; RESTART
   #:increase-iterations                ; RESTART
   #:give-up                            ; RESTART
   )
  )

(in-package #:root)

;;;;;;;;;;;;;;;;;;;;;;;;;; ERROR CONDITION ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition divergence-error (error)
  ((method :initarg :method
           :initform 'unknown
           :reader divergence-error-method)
   (iterations :initarg :iterations
               :reader divergence-error-iterations))
  (:report (lambda (condition stream)
             (format stream
                     "The method ~A did not converge at ~D iteration~:P."
                     (divergence-error-method condition)
                     (divergence-error-iterations condition))))
  (:documentation "Error to signal when a numerical method fails to converge."))


;;;;;;;;;;;;;;;;;;;;;;;;; ROOT FINDING LOGIC ;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *max-iterations* 10)
(defparameter *tolerance* 0.00001)

(defun randomly-search (f prop &key (from 0.0) (to 1.0))
  "Given a function F: REAL -> REAL, and a unary function PROP : NUMBER -> BOOL, search for a point X such that (F X) satisfies PROP. The search is done within the interval [FROM, TO), which defaults to the unit interval.

If a certain number of iterations is exceeded, then DIVERGENCE-ERROR will be signaled."
  (loop :for iteration :from 1
        :for x := (+ from (random (- to from)))
        :when (> iteration (* 10 *max-iterations*))
          :do (error 'divergence-error :iterations (1- iteration))
        :when (funcall prop (funcall f x))
          :do (return x)))

(defun bisect (f)
  "Find a root of the function F: REAL -> REAL using the bisection method.

Signals DIVERGENCE-ERROR with the method BISECT if it could not find a root."
  (loop :with x-neg := (randomly-search f #'minusp)
        :and  x-pos := (randomly-search f #'plusp)
        :for iteration :from 1
        :for x-mid := (/ (+ x-neg x-pos) 2.0)
        :when (or (> iteration *max-iterations*)
                  (= (signum x-neg) (signum x-pos)))
          :do (error 'divergence-error :iterations (1- iteration)
                                       :method 'bisect)
        :do (let ((fx (funcall f x-mid)))
              (cond
                ((< (abs fx) *tolerance*) (return x-mid))
                ((minusp fx) (setf x-neg x-mid))
                ((plusp fx)  (setf x-pos x-mid))))))

(defun derivative (f)
  "Given a function F : REAL -> REAL, return a function that approximates its derivative."
  (let ((dx *tolerance*))
    (lambda (x)
      (/ (- (funcall f (+ x dx)) (funcall f x)) dx))))

(defun newton (f &optional (df (derivative f)) (x0 (random 1.0)))
  "Find a root of the function F: REAL -> REAL using Newton's method.

This method assumes F is everywhere differentiable. The derivative of F may be passed in as the first optional argument DF. A starting point for locating the root may be passed in as the second optional argument X0.

Signals DIVERGENCE-ERROR with the method NEWTON if it could not find a root."
  (loop :for iteration :from 1
        :for x := x0 :then (- x (/ (funcall f x) (funcall df x)))
        :when (> iteration *max-iterations*)
          :do (error 'divergence-error :iterations (1- iteration)
                                       :method 'newton)
        :when (< (abs (funcall f x)) *tolerance*)
          :do (return x)))


;;;;;;;;;;;;;;;;;;;;;;;; COMPUTATION DRIVERS ;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-value ()
  (format *query-io* "Value: ")
  (finish-output *query-io*)
  (list (read *query-io*)))


(defun find-root (f)
  "Find a root of F. Return either the root, or NIL if it couldn't be found.

This function provides many restarts in the event of an error. The restarts are:

    TRY-NEWTON: Try to use Newton's method again with a different starting point.

    TRY-BISECTION: Try to use bisection.

    INCREASE-ITERATIONS: Try again, with more iterations.

    GIVE-UP: Just return NIL."
  ;; We might pretend that the DERIVATIVE calculation is _very_
  ;; expensive, and so we won't want to recompute it in the event of
  ;; an error.
  (let* ((df (derivative f))
         ;; We have a variable bound to a thunk which computes the
         ;; current method so that we can change it an arbitrary
         ;; number of times.
         (current-method (lambda () (newton f df)))
         ;; Re-bind *MAX-ITERATIONS* so that we maintain its value
         ;; outside of the dynamic scope of this LET*.
         (*max-iterations* *max-iterations*))
    (loop
      (restart-case (return-from find-root (funcall current-method))
        ;; Below are possible things to do if NEWTON fails.
        (try-newton (&optional (x0 0.5))
          :report "Try Newton's method with a new initial value."
          :interactive read-value
          (setf current-method (lambda () (newton f df x0))))
        (try-bisection ()
          :report "Try to find root using bisection."
          (setf current-method (lambda () (bisect f))))
        (increase-iterations (iterations)
          :report "Try again with more iterations."
          :interactive read-value
          (setf *max-iterations* (max 1 (round iterations))))
        (give-up ()
          :report "Give up and return NIL."
          (setf current-method (constantly nil)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; USER CODE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Below represents what a typical user of the above library might
;;; do. We keep it in the same file only for reading convenience.
;;;
;;; The user of this library is interested in computing roots of functions.
(in-package #:cl-user)


;;;;;;;;;;;;;;;;;;;;;;; SOME SAMPLE FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;

;;; Well behaved.
(defun f1 (x) (- (* x x) 0.25))


;;; Domain error.
(defun f2 (x)
  (unless (plusp x)
    (error 'floating-point-invalid-operation :operation 'f2
                                             :operands (list x)))
  (- (expt x 1/3) 0.25))

;;; Lots of zero derivatives, and Newton is easily fooled.
(defun f3 (x) (+ (* 20 x) (cos (* 20 x)) -7.8))

;;; Outrageous function with a lot going on. Root finding will
;;; probably fail unless you start your Newton step close enough. Try
;;; USE-NEWTON with X0 = 0.8.
(defun f4 (x) (sin (/ 10.0 (sin (/ x 10.0)))))

;;; A functionw with no roots!
(defun f5 (x) (1+ (exp (* x x))))


;;; I, the application programmer, want to not only decide *which*
;;; errors I service, but also *where* recovery should occur.

(defun increase-iterations-or-bust (c)
  ;; If we've exceeded 10k iterations, we might as well just go home.
  (if (< 10000 (root:divergence-error-iterations c))
      (invoke-restart 'root:increase-iterations
                      (* 10 (root:divergence-error-iterations c)))
      (invoke-restart 'root:give-up)))

;;; Non-interactive version. Will select an option automatically.
(defun give-me-a-damn-root (f)
  "Lisp will try its best in finding a root for your function at all costs."
  (handler-bind ((floating-point-invalid-operation
                   (lambda (c)
                     (declare (ignore c))
                     (invoke-restart 'root:try-bisection)))
                 (root:divergence-error
                   (lambda (c)
                     (case (root:divergence-error-method c)
                       ((newton)    (invoke-restart 'root:try-bisection))
                       ((bisection) (increase-iterations-or-bust c))
                       (otherwise   (increase-iterations-or-bust c))))))
    (root:find-root f)))
