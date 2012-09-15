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

(defun fibs ()
  "Create a new generator for Fibonacci numbers."
  (generator
   (let ((x 0)
         (y 1))
     (lambda ()
       (prog1 x
         (psetq x y
                y (+ x y)))))))


;;;;;;;;;;;;;;;;;;;;;;;; Generator Modifiers ;;;;;;;;;;;;;;;;;;;;;;;;;

(defun map-generator (f gen)
  "Produce a new generator equivalent to GEN except F is applied to
each generated value. In other words, map F across GEN, producing a
new generator."
  (generator
   (lambda ()
     (funcall f (next gen)))))