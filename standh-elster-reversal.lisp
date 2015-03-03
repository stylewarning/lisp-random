;;;; strandh-elster-reversal.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

;;;; This is an implementation of the Strandh-Elster bit reversal
;;;; algorithm. It is described in the following reference:
;;;;
;;;;     Robert Strandh and Anne C. Elster, "A Very Fast Recursive
;;;;     Bit-Reversal Algorithm", SIAM CSE'00: First SIAM Conference
;;;;     on Computational Science and Engineering, Washington, D.C.,
;;;;     Sep 21-24, 2000
;;;;
;;;; Since I do not have access to it, Prof. Strandh described it with
;;;; a reference implementation in the following link:
;;;;
;;;;     http://metamodular.com/bit-reversal.lisp
;;;;
;;;; The implementation below is motivated by the challenges set forth
;;;; at the end of that file. Succinctly, the challenges are (1) to
;;;; eliminate passing data on the stack (to allow simple CALLs), (2)
;;;; create special versions of ALL and GREATER which do not need to
;;;; check N.
;;;;
;;;; The first challenge is satisfied by
;;;; MAP-NON-SYMMETRIC-BIT-REVERSALS-GENERIC.
;;;;
;;;; The second challenge I thought was satisfied by
;;;; GENERATE-REVERSAL-DEFINITION-FOR. I actually misinterpreted the
;;;; challenge; the whole loop should *not* be unrolled. This is
;;;; totally useless but kept anyway.
;;;;
;;;; Hypothesis: Generating special versions of ALL and GREATER will
;;;; probably blast the cache away and will probably not be faster.


(declaim (optimize speed (safety 0) (debug 0) (space 0)))

;;; Challenge #1 (Local Variable Elimination)

(defmacro with-reverted-operations (bindings &body body)
  (flet ((reverted-op (form)
           (ecase (car form)
             ;; ASH is not totally reversible. (We can shift off
             ;; values that can't be recovered without saving them.)
             ((ash) `(ash ,(second form) (- ,(third form))))
             ((+) `(- ,@(rest form)))
             ((-) `(+ ,@(rest form)))
             ;; LOGIOR is not totally reversible. (We can LOGIOR 1
             ;; into 1, which which can't be recovered without saving
             ;; them.)
             ((logior) `(logxor ,@(rest form))))))
    `(progn
       ,@(loop :for (var value-form) :in bindings
               :collect `(setf ,var ,value-form))
       ,@body
       ,@(loop :for (var value-form) :in bindings
               :collect `(setf ,var ,(reverted-op value-form)))
       (values))))

(defun map-non-symmetric-bit-reversals-generic (bits f)
  "Call the binary function F on numbers all numbers A and B such that:

    * A < B;
    * The bits of B are the reversal of the bits of A;
    * A and B are BITS or fewer bits wide.

Symmetric A and B are not included, and are not needed for most bit-reversal applications."
  (declare (type (integer 0 64) bits))
  (let ((n (- bits 2))                  ; This can be eliminated.
        (a1 0)
        (a2 0)
        (b1 0)
        (b2 0))
    (declare (type (integer 0 64) n)
             (type (unsigned-byte 64) a1 a2 b1 b2)
             (type (function (t t)) f))
    (labels
        ((all ()
           (if (zerop n)
               (progn
                 (funcall f a1 a2)
                 (values))
               (with-reverted-operations ((b1 (ash b1 1))
                                          (b2 (ash b2 -1)))
                 (if (= n 1)
                     (with-reverted-operations ((n (- n 1)))
                       (all)
                       (with-reverted-operations ((a1 (logior a1 b1))
                                                  (a2 (logior a2 b2)))
                         (all)))
                     (with-reverted-operations ((n (- n 2)))
                       (all)

                       (with-reverted-operations ((a1 (logior a1 b1))
                                                  (a2 (logior a2 b2)))
                         (all))

                       (with-reverted-operations ((a1 (logior a1 b1 b2))
                                                  (a2 (logior a2 b1 b2)))
                         (all))

                       (with-reverted-operations ((a1 (logior a1 b2))
                                                  (a2 (logior a2 b1)))
                         (all)))))))
         (greater ()
           (with-reverted-operations ((b1 (ash b1 1))
                                      (b2 (ash b2 -1))
                                      (n (- n 2)))
             (if (< n 4)
                 (with-reverted-operations ((a1 (logior a1 b1))
                                            (a2 (logior a2 b2)))
                   (all))
                 (progn
                   (greater)
                   (with-reverted-operations ((a1 (logior a1 b1))
                                              (a2 (logior a2 b2)))
                     (all))
                   (with-reverted-operations ((a1 (logior a1 b1 b2))
                                                  (a2 (logior a2 b1 b2)))
                     (greater)))))))
      (setf a1 0
            a2 0
            b1 1
            b2 (ash 1 (1+ n)))
      
      (greater)

      (setf b1 1
            b2 (ash 1 (1+ n))
            a1 b1
            a2 b2)
      
      (all)

      (setf b1 1
            b2 (ash 1 (1+ n))
            a1 (logior b1 b2)
            a2 a1)
      
      (greater)
      
      ;; Return nothing.
      (values))))

(defun print-bit-reversal (n)
  (flet ((print-pair (a b)
           (format t "~v,'0b   ~v,'0b~%" n a n b)))
    (map-non-symmetric-bit-reversals-generic n #'print-pair)))

(defun bit-reversed-permute! (x)
  "Permute the simple vector X of length 2^N in bit reversed order."
  (declare (type simple-vector x))
  (let* ((length (length x))
         (bits (integer-length (max 0 (1- length)))))
    ;; Check that this is a power of two.
    (assert (zerop (logand length (1- length))))
    (flet ((swap (a b)
             (rotatef (aref x a) (aref x b))))
      (declare (dynamic-extent #'swap))
      (map-non-symmetric-bit-reversals-generic bits #'swap)
      x)))



;;; (Not Really) Challenge #2 (Unrolling) (Kept for posterity)
;;;
;;; This was incredibly dumb and useless.
;;;
;;; (It does reveal however that SBCL can produce kind of sloppy ASM
;;; that is ripe for peephole optimization.)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun generate-non-symmetric-reversal-code (bits form a b)
    "Generate unrolled code which will do the equivalent of MAP-NON-SYMMETRIC-BIT-REVERSALS-GENERIC for a fixed width BITS.

FORM should be a form that uses the symbols A and B. A and B will be bound when FORM is evaluated to the values specified by the documentation of MAP-NON-SYMMETRIC-BIT-REVERSALS-GENERIC. An example form might be:

    (rotatef (aref x a) (aref x b)).
"
    (let ((n (- bits 2)))
      `(let ((a1 0)
             (a2 0)
             (b1 0)
             (b2 0))
         ,(labels
              ((generate-all (n)
                 (if (zerop n)
                     `(let ((,a a1)
                            (,b a2))
                        ,form
                        (values))
                     `(with-reverted-operations ((b1 (ash b1 1))
                                                 (b2 (ash b2 -1)))
                        ,(if (= n 1)
                             `(progn
                                ,(generate-all (- n 1))
                                (with-reverted-operations ((a1 (logior a1 b1))
                                                           (a2 (logior a2 b2)))
                                  ,(generate-all (- n 1))))
                             `(progn
                                ,(generate-all (- n 2))
                                
                                (with-reverted-operations ((a1 (logior a1 b1))
                                                           (a2 (logior a2 b2)))
                                  ,(generate-all (- n 2)))
                                
                                (with-reverted-operations ((a1 (logior a1 b1 b2))
                                                           (a2 (logior a2 b1 b2)))
                                  ,(generate-all (- n 2)))
                                
                                (with-reverted-operations ((a1 (logior a1 b2))
                                                           (a2 (logior a2 b1)))
                                  ,(generate-all (- n 2))))))))
               (generate-greater (n)
                 `(with-reverted-operations ((b1 (ash b1 1))
                                             (b2 (ash b2 -1)))
                    ,(if (< (- n 2) 4)
                         `(with-reverted-operations ((a1 (logior a1 b1))
                                                     (a2 (logior a2 b2)))
                            ,(generate-all (- n 2)))
                         `(progn
                            ,(generate-greater (- n 2))
                            (with-reverted-operations ((a1 (logior a1 b1))
                                                       (a2 (logior a2 b2)))
                              ,(generate-all (- n 2)))
                            (with-reverted-operations ((a1 (logior a1 b1 b2))
                                                       (a2 (logior a2 b1 b2)))
                              ,(generate-greater (- n 2))))))))
            `(progn
               (setf a1 0
                     a2 0
                     b1 1
                     b2 ,(ash 1 (1+ n)))
               
               ,(generate-greater n)

               (setf b1 1
                     b2 ,(ash 1 (1+ n))
                     a1 b1
                     a2 b2)
               
               ,(generate-all n)

               (setf b1 1
                     b2 ,(ash 1 (1+ n))
                     a1 (logior b1 b2)
                     a2 a1)

               ,(generate-greater n)    ; This code was already
                                        ; generated above.
               
               ;; Return nothing.
               (values))))))
  
  (defun generate-reversal-definition-for (bits)
    (let ((name (intern (format nil "MAP-NON-SYMMETRIC-BIT-REVERSALS-~D" bits))))
      `(defun ,name (f)
         ,(generate-non-symmetric-reversal-code bits
                                                '(funcall f a b)
                                                'a
                                                'b)))))

(macrolet ((define-non-symmetric-reversal-mapper (n)
             (generate-reversal-definition-for n)))
  (define-non-symmetric-reversal-mapper 4))


(macrolet ((define-permutation-function (bits)
             (let ((name (intern (format nil "BIT-REVERSED-PERMUTE-~D" bits))))
               `(defun ,name (x)
                  (declare (type (simple-vector ,(expt 2 bits)) x))
                  ,(generate-non-symmetric-reversal-code
                    bits
                    '(rotatef (aref x a) (aref x b))
                    'a
                    'b)))))
  (define-permutation-function 4))

;;; For testing...

(defun tree-size (a)
  (let ((size 0))
    (tree-equal a a :test (lambda (x y)
                            (incf size)
                            (eq x y)))
    size))
