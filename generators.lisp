;;;; generators.lisp
;;;;
;;;; Copyright (c) 2021 Robert Smith

(ql:quickload :chanl)

;;; An implementation of Python-style generators using threads.
;;;
;;; In the future, we might decide to instead use an actual class
;;; instead of just lambda functions.

(define-condition generator-exhausted (error)
  ()
  (:documentation "Signal that a generator is exhausted."))

(defun make-generator (function)
  (let* ((thread nil)
         (request-chan (make-instance 'chanl:bounded-channel :size 1))
         (return-chan (make-instance 'chanl:bounded-channel :size 1))
         (done nil)
         (finished-sentinel (gensym "FINISHED")))
    (labels ((yield (return-value)
               (chanl:send return-chan return-value)
               (ecase (chanl:recv request-chan)
                 (next nil)))

             (thread-function ()
               (unwind-protect (funcall function #'yield)
                 (yield finished-sentinel)))

             (generator-function ()
               (cond
                 (done
                  (error 'generator-exhausted))
                 (t
                  (when (null thread)
                    (setf thread (bt:make-thread #'thread-function)))
                  (chanl:send request-chan 'next)
                  (let ((value (chanl:recv return-chan)))
                    (when (eq finished-sentinel value)
                      (setf done t)
                      (error 'generator-exhausted))
                    value)))))
      (sb-ext:finalize #'generator-function (lambda ()
                                              (when (and (not (null thread))
                                                         (bt:thread-alive-p thread))
                                                (bt:destroy-thread thread))))
      #'generator-function)))

(defmacro define-generator (name args &body body)
  (let ((yield-fn (gensym "YIELD-AN"))
        (value    (gensym "VALUE")))
    `(defun ,name ,args
       (make-generator (lambda (,yield-fn)
                         (flet ((yield (,value)
                                  (funcall ,yield-fn ,value)))
                           ,@body))))))


;;; Examples

(define-generator naturals ()
  (let ((now 0))
    (loop (yield (incf now)))))

(define-generator integers ()
  (yield 0)
  (let ((naturals (naturals)))
    (loop :for n := (funcall naturals)
          :do (yield n)
              (yield (- n)))))

(define-generator range (a b)
  (loop :for x :from a :below b :do (yield x)))

(define-generator seq (sequence)
  (map nil #'yield sequence))

(define-generator cycle (sequence)
  (loop
    (map nil #'yield sequence)))

(defun next-or (gen else)
  "Return the next element of the generator GEN, or ELSE if it's exhausted."
  (handler-case (funcall gen)
    (generator-exhausted (c)
      (declare (ignore c))
      else)))

(defun map-generator (f gen)
  "Map the unary function F across all elements of the generator GEN."
  (handler-case (loop
                  (funcall f (funcall gen)))
    (generator-exhausted (c)
      (declare (ignore c))
      nil)))

(defun generator-list (gen)
  "Convert a generator GEN into a list."
  (loop :with done := '#:done
        :for x := (next-or gen done)
        :when (eq done x)
          :do (loop-finish)
        :collect x))

;;; Famous SAME-FRINGE problem:
;;;
;;; Are the leaves of two trees the same?

(define-generator leaves (tree)
  (labels ((explore (node)
             (cond
               ((consp node)
                (explore (car node))
                (explore (cdr node)))
               (t
                (yield node)))))
    (explore tree)))

(defun same-fringe (a b &key (test 'eql))
  "Do the trees A and B have the same fringe?"
  (loop :with done := '#:done
        :with leaves-a := (leaves a)
        :with leaves-b := (leaves b)
        :for next-a := (next-or leaves-a done)
        :for next-b := (next-or leaves-b done)
        :do (when (and (eq done next-a)
                       (eq done next-b))
              (return t))
            (unless (funcall test next-a next-b)
              (return nil))))


;;; Stress test

(defun stress (&optional (n 10000))
  (let ((generators (loop :repeat n
                          :collect (naturals))))
    (loop :repeat n :do (map nil #'funcall generators))))
