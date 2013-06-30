;;;; jump.lisp
;;;; Copyright (c) 2013 Robert Smith

(declaim (optimize speed (safety 0) (debug 0) (space 0)))

;;;; Implementation of a sort of jump table.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ensure-list (x)
    "Ensure that X is a list."
    (if (listp x)
        x
        (list x)))

  (defun contains-below-n (numbers)
    "Ensure that NUMBERS contains the all of the integers from 0
  to (1- (LENGTH NUMBERS))."
    (let ((sorted (sort (copy-list numbers) #'<)))
      (loop :for i :from 0
            :for x :in sorted
            :always (= i x)))))

(defmacro constant-load-time-value (value)
  `(load-time-value ,value t))


;;; XXX: This doesn't support referencing the lexical environment.
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
                              (and (listp (car jump-case))
                                   (every #'integerp (car jump-case))
                                   (notany #'minusp (car jump-case))))))
                 cases)
          (cases)
          "The cases must be lists whose first element is a positive integer or list of positive integers.")
  (let ((table    (make-hash-table))
        (bindings nil)
        (maxval   0))
    (labels ((populate-table ()
               (dolist (jump-case cases)
                 (let ((indexes (car jump-case))
                       (binding-var (gensym "CASE-"))
                       (lambda-form `(lambda () ,@(cdr jump-case))))
                   (push (list binding-var lambda-form) bindings)
                   (dolist (index (ensure-list indexes))
                     (when (> index maxval)
                       (setf maxval index))
                     (setf (gethash index table) binding-var)))))
             
             (table-to-initial-contents ()
               (let ((default-var (gensym "DEFAULT-")))
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
             (aref (constant-load-time-value
                    (make-array ,(1+ maxval)
                                :element-type '(function () t)
                                :initial-contents
                                ,(table-to-initial-contents)))
                   ,n))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Binary Search ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun generate-search (numbers &optional code-dict)
    (let ((var (gensym "N"))
          (otherwise (gensym "OTHERWISE-")))
      (labels ((lookup (n)
                 (gethash n code-dict))
               
               (split (numbers)
                 (let ((len/2 (ceiling (length numbers) 2)))
                   (values (subseq numbers 0 len/2)
                           (subseq numbers len/2))))
               
               (rec (numbers)
                 (cond
                   ((null numbers)
                    (error "We got a null list..."))

                   ((null (rest numbers))
                    `(if (= ,var ,(first numbers))
                         (progn ,@(lookup (first numbers)))
                         (,otherwise)))
                   
                   (t
                    (multiple-value-bind (left right)
                        (split numbers)
                      (let ((left-min (first left))
                            (left-max (car (last left)))
                            (right-min (first right))
                            (right-max (car (last right))))
                        (if (and (= left-min left-max)
                                 (= right-min right-max)) ; should always be true
                            `(cond
                               ((= ,var ,left-min) ,@(lookup left-min))
                               ((= ,var ,right-min) ,@(lookup right-min))
                               (t (,otherwise)))
                            `(if (<= ,left-min ,var ,left-max)
                                 ,(rec left)
                                 ,(rec right)))))))))
        (let* ((sorted (sort (copy-list numbers) #'<))
               (min (first sorted))
               (max (car (last sorted))))
          `(lambda (,var)
             ,(if (= min max)
                  `(if (= ,var ,min)
                       (progn ,@(lookup min))
                       (progn ,@(lookup t)))
                  `(flet ((,otherwise ()
                            ,@(lookup t)))
                     (declare (dynamic-extent (function ,otherwise)))
                     (if (<= ,min ,var ,max)
                         ,(rec sorted)
                         (,otherwise))))))))))

(defmacro binary-case (n &body cases)
  (let ((code-dict (make-hash-table)))
    (labels ((extract-keys ()
               (mapcan (lambda (c)
                         (let ((key (car c)))
                           (typecase key
                             (list (copy-list key))
                             (integer (list key))
                             (t nil))))
                       cases))
             
             (populate-code-dict ()
               (loop :for (keys . body) :in cases
                     :do (etypecase keys
                           ((member t otherwise)
                            (setf (gethash t code-dict) body))
                           
                           (integer
                            (setf (gethash keys code-dict) body))
                           
                           (list
                            (dolist (key keys)
                              (setf (gethash key code-dict) body)))))))
      
      (populate-code-dict)
      
      (unless (gethash t code-dict)
        (setf (gethash t code-dict) (list nil)))
      
      `(funcall ,(generate-search (extract-keys) code-dict)
                ,n))))

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

(defmacro generate-binary-case-form (num-cases)
  `(binary-case (random ,num-cases)
     ,@(loop :for i :below num-cases
             :collect (list i i))))

(defun test-jump ()
  (loop :repeat *trials*
        :do (generate-jump-form 1000)))

(defun test-case ()
  (loop :repeat *trials*
        :do (generate-case-form 1000)))

(defun test-binary-case ()
  (loop :repeat *trials*
        :do (generate-binary-case-form 1000)))


;; CL-USER> (progn
;;            (time (test-jump))
;;            (time (test-case))
;;            (time (test-binary-case)))
;; Evaluation took:
;;   0.600 seconds of real time
;;   0.601523 seconds of total run time (0.599077 user, 0.002446 system)
;;   100.33% CPU
;;   1,017,956,287 processor cycles
;;   11,008 bytes consed
;;
;; Evaluation took:
;;   4.777 seconds of real time
;;   4.786960 seconds of total run time (4.768514 user, 0.018446 system)
;;   100.21% CPU
;;   8,103,027,934 processor cycles
;;   62,304 bytes consed
;;
;; Evaluation took:
;;   0.776 seconds of real time
;;   0.777397 seconds of total run time (0.774449 user, 0.002948 system)
;;   100.13% CPU
;;   1,315,515,748 processor cycles
;;   0 bytes consed


;;;;;;;;;;;;;;;;;;;;;;;;;;; STATE MACHINE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun run-machine ()
  (loop :named state-machine
        :with previous-state := nil
        :and state := 0
        :do (macrolet ((next (n)
                         `(progn
                            (psetf previous-state state
                                   state ,n)
                            (return-from next-state)))
                       (done (return-val)
                         `(return-from state-machine ,return-val)))
              (block next-state
                (format t "~A --> ~A: " previous-state state)
                (tagbody
                   (case state
                     (0       (go :TAG-ZERO))
                     (1       (go :TAG-ONE))
                     ((2 4 6) (go :TAG-TWO))
                     ((3 5)   (go :TAG-THREE))
                     (7       (go :TAG-SEVEN)))
                 :TAG-ZERO
                   (format t "Hello!~%")
                   (next 1)
                 :TAG-ONE
                   (format t "World!~%")
                   (next 2)
                 :TAG-TWO
                   (format t "even!~%")
                   (next (random 8))
                 :TAG-THREE
                   (format t "odd!~%")
                   (next (random 8))
                 :TAG-SEVEN
                   (format t "done!~%")
                   (done t))))))
