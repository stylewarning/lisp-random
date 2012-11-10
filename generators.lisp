;;;; generators.lisp
;;;; Copyright (c) 2012 Robert Smith

;;;; Some trivial generators


;;;;;;;;;;;;;;;;;;;;;;;;;; Error conditions ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition generator-exhausted (error) ()
  (:report "Generator has been exhausted."))

(declaim (inline exhausted))
(defun exhausted ()
  (restart-case (error 'generator-exhausted)
    (return-nil ()
      :report "Return NIL."
      nil)
    
    (specify-value (value)
      :report "Specify a value to return."
      :interactive (lambda ()
                     (format t "Enter a value (unevaluated): ")
                     (list (read)))
      value)))


;;;;;;;;;;;;;;;;;;;;;;;;;;; Data Structure ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (generator (:print-function
                       (lambda (obj str depth)
                         (declare (ignore depth))
                         (print-unreadable-object (obj str :type t
                                                           :identity t))))
                      (:copier nil)
                      (:predicate generatorp)
                      (:constructor generator (closure)))
  (closure #'exhausted :type (function () t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Collectors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline next))
(defun next (gen)
  "Obtain the next value in a generator GEN."
  (funcall (generator-closure gen)))

(defun next-if (predicate gen)
  "Continue obtaining elements from the generator GEN until one
  satisfies the predicate PREDICATE."
  (loop :for x := (next gen) :then (next gen)
        :until (funcall predicate x)
        :do (values)
        :finally (return x)))

(defun collect (gen)
  "Collect all of the values of the generator GEN into a list."
  (let ((collected nil))
    (loop
      (handler-case (push (next gen)
                          collected)
        (generator-exhausted ()
          (return (nreverse collected)))))))

(defun take (n gen)
  "Collect at most N values of the generator GEN into a list."
  (let ((collected nil))
    (loop :repeat n
          :do (handler-case (push (next gen)
                                  collected)
                (generator-exhausted ()
                  (return (nreverse collected))))
          :finally (return (nreverse collected)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Iterators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xrange (a b &key (step 1) key)
  "Return a generator that returns values from A to B, with a step
size of STEP. Optionally provide KEY which is applied to each
generated element."
  (generator
   (let ((x a)
         (key (or key #'identity)))
     (lambda ()
       (if (> x b)
           (exhausted)
           (prog1 (funcall key x)
             (incf x step)))))))

(defun each (list)
  "Return a generator that iterates through the list LIST."
  (generator
   (lambda ()
     (if (null list)
         (exhausted)
         (prog1 (car list)
           (setf list (cdr list)))))))

(defun each-of (&rest items)
  "Return a generator that iterates through each of the arguments
ITEMS."
  (each items))


;;;;;;;;;;;;;;;;;;;;;;;; Generator Modifiers ;;;;;;;;;;;;;;;;;;;;;;;;;

(defun map-generator (f gen)
  "Produce a new generator equivalent to GEN except F is applied to
each generated value. In other words, map F across GEN, producing a
new generator."
  (generator
   (lambda ()
     (funcall f (next gen)))))

(defun accumulator (f init gen)
  "Accumulate values from GEN according to the rule
   x(0)   = init
   x(n+1) = f( next(g), x(n) )

Essentially a generator form of REDUCE or fold."
  (generator
   (let ((accum init))
     (lambda ()
       (prog1 accum
         (setq accum (funcall f (next gen) accum)))))))

(defun filter (predicate gen)
  "Produce a new generator which keeps elements of GEN that satisfy
  the predicate PREDICATE."
  (generator
   (lambda ()
     (next-if predicate gen))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Files ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun file-lines (filename)
  "Create a new generator for the lines of a file."
  (generator
   (let ((stream (open filename :direction :input
                                :if-does-not-exist :error)))
     (lambda ()
       (let ((line (read-line stream nil nil)))
         (or line
             (progn
               (close stream)
               (exhausted))))))))

;;;;;;;;;;;;;;;;;;;;;; Miscellaneous Generators ;;;;;;;;;;;;;;;;;;;;;;

(defun fibs ()
  "Create a new generator for Fibonacci numbers."
  (generator
   (let ((x 0)
         (y 1))
     (lambda ()
       (prog1 x
         (psetq x y
                y (+ x y)))))))

(defun up-from (n &key (step 1) key)
  "Generate integers starting at N and increasing by STEP, optionally
calling KEY on each element.."
  (generator
   (let ((key (or key #'identity)))
     (lambda ()
       (prog1 (funcall key n)
         (incf n step))))))
