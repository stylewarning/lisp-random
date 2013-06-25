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

(defmacro jump (n &body cases)
  "Efficiently execute the body of a case in CASES and return
depending on the value of N.

N should be a non-negative integer below the total number of cases. The syntax of a case is

    (NUM FORM*)

where NUM is an integer having the same values as N.

Example:

    (jump (random 3)
      (0 (print 0) 'zero)
      (1 (print 1) 'one)
      (2 (print 2) 'two))
"
  (assert (not (null cases))
          (cases)
          "There must be at least one case, but none were given.")
  (assert (every #'(lambda (jump-case)
                     (and (listp jump-case)
                          (not (null jump-case))
                          (integerp (car jump-case))
                          (not (minusp (car jump-case)))))
                 cases)
          (cases)
          "The cases must be lists whose first element is a positive integer.")
  (assert (contains-below-n (mapcar #'car cases))
          (cases)
          "The cases must contain all integers between 0 and ~A inclusive."
          (1- (length cases)))
  `(funcall
    (the (function () t)
         (svref (constant-load-time-value
                 (make-array ,(length cases)
                             :element-type '(function () t)
                             :initial-contents
                             ,(cons 'list
                                    (loop :for i :below (length cases)
                                          :collect `(lambda ()
                                                      ,@(cdr (assoc i cases)))))))
                ,n))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *trials* 10000000)

(defmacro generate-test-form (name num-cases)
  `(,name (random ,num-cases)
          ,@(loop :for i :below num-cases
                  :collect (list i i))))

(defun test-jump ()
  (loop :repeat *trials*
        :do (generate-test-form jump 1000)))

(defun test-case ()
  (loop :repeat *trials*
        :do (generate-test-form case 1000)))

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
