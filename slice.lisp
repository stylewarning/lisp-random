;;;; slice.lisp
;;;;
;;;; Implements "array slicing" (more specifically, extracting copies
;;;; of "sub-arrays" of a Common Lisp array).
;;;;
;;;; Could be optimized with compiler macros.

;;; Examples:
;;;
;;; CL-USER> (defvar *a '#3A((("000" "001" "002")
;;;                           ("010" "011" "012"))
;;;                          (("100" "101" "102")
;;;                           ("110" "111" "112"))
;;;                          (("200" "201" "202")
;;;                           ("210" "211" "212"))
;;;                          (("300" "301" "302")
;;;                           ("310" "311" "312"))))
;;; *A
;;;
;;; CL-USER> (slice *a 0 0 1)
;;; "001"
;;;
;;; CL-USER> (slice *a 0 0 '*)
;;; #("000" "001" "002")
;;;
;;; CL-USER> (slice *a 0 0)
;;; #("000" "001" "002")
;;;
;;; CL-USER> (slice *a 0 0 '(1 . 2))
;;; "001"
;;;
;;; CL-USER> (slice *a 0 0 '(1 . 3))
;;; #("001" "002")
;;;
;;; CL-USER> (slice *a 0 '* 0)
;;; #("000" "010")
;;;
;;; CL-USER> (slice *a '* '* 0)
;;; #2A(("000" "100") ("200" "300") ("010" "110") ("210" "310"))
;;;
;;; CL-USER> (slice *a '(1 . *) '* '(1 . *))
;;; #3A((("101" "201") ("301" "111"))
;;;     (("211" "311") ("102" "202"))
;;;     (("302" "112") ("212" "312")))


(defun slice (array &rest components)
  "Extract a slice from the rank-N array ARRAY based off of COMPONENTS.

COMPONENTS specifies which elements to select from ARRAY. COMPONENTS
are selectors written in row-major order. The number of components
must not exceed the rank of ARRAY.

COMPONENTS have the following forms. (In the following, suppose we are
working with the Ith component of a rank-N array.)

- An integer K

This means to select all elements of ARRAY whose Ith index is K.

- The symbol CL:*

This means to select all elements of ARRAY whose Ith index is any
valid index of ARRAY. This will be 0 to (ARRAY-DIMENSION ARRAY I).

- The cons (A . B) with integers A and B

This means to select all elements of ARRAY whose Ith index is any
valid index K such that 0 <= A <= K < B <= (ARRAY-DIMENSION ARRAY I).

- The cons (A . CL:*) with an integer A

This means to select all elements of ARRAY whose Ith index is any
valid index K such that 0 <= A <= K < (ARRAY-DIMENSION ARRAY I).

- The cons (CL:* . B) with an integer B

This is equivalent to having specified (0 . B).

- The cons (CL:* . CL:*)

This is equivalent to having specified the symbol CL:*.

- Unspecified (i.e., fewer COMPONENTS than N)

This is equivalent to having specified the symbol CL:*.

The return value is either

- A value from ARRAY (in the case that the rank of the result would be
  zero)

- An array of rank M (where M is the number of non-degenerate axes).
"
  (let ((rank (array-rank array))
        (num-components (length components))
        ;; will be a list of RANK conses
        (selectors nil))
    (when (> num-components rank)
      (error "Too many components"))

    ;; Bail out on some easy cases:
    ;;
    ;; 1. Select everything.
    (when (null components)
      (return-from slice array))

    ;; 2. Select a single element.
    (when (and (every #'integerp components)
               (= num-components rank))
      (return-from slice (apply #'aref array components)))

    ;; Collect the selectors.
    (loop :for index :from 0 :below rank
          :for dim := (array-dimension array index)
          :if (null components)
            :do (push (cons 0 dim) selectors)
          :else
            :do (push (let ((sel (pop components)))
                        (etypecase sel
                          ((or (member *)
                               (cons (member *) (member *)))
                           (cons 0 dim))
                          ((cons integer integer)
                           (unless (< (car sel) (cdr sel))
                             (error "invalid component: ~A" sel))
                           (unless (<= 0 (car sel) (1- dim))
                             (error "invalid lower bound: ~A" sel))
                           (unless (<= 1 (cdr sel) dim)
                             (error "invalid upper bound: ~A" sel))
                           sel)
                          ((cons integer (member *))
                           (unless (<= 0 (car sel) (1- dim))
                             (error "invalid lower bound: ~A" sel))
                           (cons (car sel) dim))
                          ((cons (member *) integer)
                           (unless (<= 1 (cdr sel) dim)
                             (error "invalid upper bound: ~A" sel))
                           (cons 0 (cdr sel)))
                          (integer
                           (unless (<= 0 sel (1- rank))
                             (error "invalid component: ~A" sel))
                           (cons sel (1+ sel)))))
                      selectors)
          :finally (setf selectors (nreverse selectors)))

    ;; Figure out the target array.
    (let* ((target-dims (loop :for sel :in selectors
                              :collect (- (cdr sel) (car sel))))
           (target-dims-reduced (remove 1 target-dims))
           (num-target-elements (reduce #'* target-dims-reduced :initial-value 1))
           (target (make-array target-dims-reduced :element-type (array-element-type array))))
      (labels ((index->accessor (i radices selectors accessors)
                 (if (null radices)
                     (nreverse accessors)
                     (multiple-value-bind (quo rem) (floor i (car radices))
                       (index->accessor quo
                                        (cdr radices)
                                        (cdr selectors)
                                        (cons (+ rem (caar selectors)) accessors))))))

        ;; Fill the elements.
        (dotimes (i num-target-elements)
          (setf (row-major-aref target i)
                (apply #'aref array (index->accessor i target-dims selectors nil))))

        ;; Return the array.
        (if (zerop (array-rank target))
            (aref target)
            target)))))
