;;;; jump.lisp
;;;; Copyright (c) 2013 Robert Smith

(declaim (optimize speed (safety 0) (debug 0) (space 0)))

;;;; Implementation of a sort of jump table.

(defmacro constant-load-time-value (value)
  `(load-time-value ,value t))

(defun contains-below-n (numbers)
  "Ensure that NUMBERS contains the all of the integers from 0
  to (1- (LENGTH NUMBERS))."
  (let ((sorted (sort (copy-list numbers) #'<)))
    (loop :for i :from 0
          :for x :in sorted
          :always (= i x))))

(defmacro jump ((n &optional default) &body cases)
  "Efficiently execute the body of a case in CASES chosen by the
non-negative integer N. If there is no case matching N and N is less
than or equal to the maximum possible case outlined in CASES, then
return DEFAULT, which is only evaluated if returned.

Syntax:

    jump-form := (JUMP (<expr> <expr>?) <case>+)
    case-form := (<value-form> <expr>*)
    value-form := <non-negative integer>
                | (<non-negative integer>+)

Example:

    (jump (5 'nope)
      (0 'zero)
      ((1 3 5) 'odd)
      ((2 4 6) 'even)
      (7 'prime))
"
  (assert (not (null cases))
          (cases)
          "There must be at least one case, but none were given.")
  (assert (every #'(lambda (jump-case)
                     (and (listp jump-case)
                          (not (null jump-case))
                          (or (and (integerp (car jump-case))
                                   (not (minusp (car jump-case))))
                              (and (every #'integerp (car jump-case))
                                   (notany #'minusp (car jump-case))))))
                 cases)
          (cases)
          "The cases must be lists whose first element is a positive integer or list of positive integers.")
  (let ((table    (make-hash-table))
        (bindings nil)
        (maxval   0))
    (labels ((ensure-list (x)
               (if (listp x)
                   x
                   (list x)))
             
             (populate-table ()
               (dolist (jump-case cases)
                 (let ((indexes (car jump-case))
                       (binding-var (gensym))
                       (lambda-form `(lambda () ,@(cdr jump-case))))
                   (push (list binding-var lambda-form) bindings)
                   (dolist (index (ensure-list indexes))
                     (when (> index maxval)
                       (setf maxval index))
                     (setf (gethash index table) binding-var)))))
             
             (table-to-initial-contents ()
               (let ((default-var (gensym "DEFAULT-VAR-")))
                 `(let* ((,default-var (lambda () ,default))
                        ,@bindings)
                    (declare (ignorable ,default-var))
                    ,(cons 'list
                           (loop :for i :to maxval
                                 :collect (if (gethash i table)
                                              (gethash i table)
                                              default-var)))))))
      
      (populate-table)
      
      `(funcall
        (the (function () t)
             (svref (constant-load-time-value
                     (make-array ,(1+ maxval)
                                 :element-type '(function () t)
                                 :initial-contents
                                 ,(table-to-initial-contents)))
                    ,n))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *trials* 10000000)

(defmacro generate-jump-form (num-cases)
  `(jump ((random ,num-cases))
     ,@(loop :for i :below num-cases
             :collect (list i i))))

(defmacro generate-case-form (num-cases)
  `(case (random ,num-cases)
     ,@(loop :for i :below num-cases
             :collect (list i i))))

(defun test-jump ()
  (loop :repeat *trials*
        :do (generate-jump-form 1000)))

(defun test-case ()
  (loop :repeat *trials*
        :do (generate-case-form 1000)))

;; CL-USER> (progn
;;            (time (test-jump))
;;            (time (test-case)))
;; Evaluation took:
;;   0.567 seconds of real time
;;   0.567869 seconds of total run time (0.565772 user, 0.002097 system)
;;   100.18% CPU
;;   960,677,725 processor cycles
;;   0 bytes consed
;;
;; Evaluation took:
;;   4.753 seconds of real time
;;   4.761654 seconds of total run time (4.743117 user, 0.018537 system)
;;   100.19% CPU
;;   8,061,858,720 processor cycles
;;   16,272 bytes consed
