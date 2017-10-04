;;;; jump.lisp
;;;;
;;;; Copyright (c) 2013-2017 Robert Smith

;(declaim (optimize speed (safety 0) (debug 0) (space 0)))

(declaim (optimize (speed 0) safety debug))

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

;;;;;;;;;;;;;;;;;;;;;;;;; GO-Dispatch Table ;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (#+sbcl sb-ext:defglobal
   #+ccl defstatic **dispatchers** (make-array 15 :adjustable t
                                                   :fill-pointer 0)))

(defmacro hop (n &body cases)
  (let* ((block-name (gensym "BLOCK-"))
         (table-name (gensym "TABLE-"))
         (static-store (gensym "STATIC-STORE-"))
         (num-cases (length cases))
         (case-labels (loop :repeat num-cases :collect (gensym "LABEL-"))))
    `(block ,block-name
       (tagbody
          (let ((,table-name
                  (let* ((,static-store (load-time-value
                                         (make-array ,num-cases
                                                     :initial-element nil))))
                    (declare (type (simple-vector ,num-cases) ,static-store))
                    (when (null (svref ,static-store 0))
                      ,@(loop :for i :from 0
                              :for label :in case-labels
                              :collect `(setf (svref ,static-store ,i)
                                              (lambda () (go ,label)))))
                    ,static-store)))
            (declare (type (simple-vector ,num-cases)))
            (funcall (the (function ()) (svref ,table-name ,n))))
          ,@(loop :for case :in cases
                  :for label :in case-labels
                  :append `(,label
                            (return-from ,block-name
                              (progn
                                ,@case))))))))


(defun test-small-hop (x)
  (let ((s 0))
    (hop x
      ((setf s 0))
      ((setf s 1))
      ((setf s 2)))
    s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type fixnum *trials*))
(defparameter *trials* 10000000)

(defmacro generate-jump-form (var x num-cases)
  `(jump (,x)
     ,@(loop :for i :below num-cases
             :collect (list i `(setf ,var ,i)))))

(defmacro generate-case-form (var x num-cases)
  `(case ,x
     ,@(loop :for i :below num-cases
             :collect (list i `(setf ,var ,i)))))

(defmacro generate-binary-case-form (var x num-cases)
  `(binary-case ,x
     ,@(loop :for i :below num-cases
             :collect (list i `(setf ,var ,i)))))

(defmacro generate-hop-case-form (var x num-cases)
  `(hop ,x
     ,@(loop :for i :below num-cases
             :collect (list `(setf ,var ,i)))))


#+ignore
(defun big-jump (x)
  (declare (type fixnum x))
  (let ((i 0))
    (generate-jump-form i x 100)
    i))

(defun big-case (x)
  (declare (type fixnum x))
  (let ((i 0))
    (generate-case-form i x 100)
    i))

(defun big-binary-case (x)
  (declare (type fixnum x))
  (let ((i 0))
    (generate-binary-case-form i x 100)
    i))

(defun big-hop (x)
  (declare (type fixnum x))
  (let ((i 0))
    (generate-hop-case-form i x 100)
    i))

#+ignore
(defun test-jump ()
  (big-jump 0)
  (dotimes (j *trials*)
    (declare (type fixnum j))
    (big-jump (random 100))))

(defun init ()
  (big-case 0)
  (big-binary-case 0)
  (big-hop 0))

(defun test-case ()
  (dotimes (j *trials*)
    (declare (type fixnum j))
    (big-case (random 5))))

(defun test-binary-case ()
  (dotimes (j *trials*)
    (declare (type fixnum j))
    (big-binary-case (random 5))))

(defun test-hop ()
  (dotimes (j *trials*)
    (declare (type fixnum j))
    (big-hop (random 5))))


;; CL-USER> (progn
;;            (time (test-jump))
;;            (time (test-case))
;;            (time (test-binary-case))
;;            (time (test-hop)))
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
