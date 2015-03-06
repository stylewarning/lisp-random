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
;;;; The second challenge is not satisfied yet.
;;;;
;;;; Hypothesis: Generating special versions of ALL and GREATER will
;;;; probably blast the cache away and will probably not be faster.


(declaim (optimize speed (safety 0) (debug 0) (space 0)))
;(declaim (optimize (speed 0) (safety 3) (debug 3)))

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

(declaim (inline map-non-symmetric-bit-reversals-generic))
(defun map-non-symmetric-bit-reversals-generic (n f)
  "Call the binary function F on numbers all numbers A and B such that:

    * A < B;
    * The bits of B are the reversal of the bits of A;
    * A and B are N or fewer bits wide.

Symmetric A and B are not included, and are not needed for most bit-reversal applications."
  (declare (optimize #+lispworks (fixnum-safety 0))
           (type (integer 0 64) n))
  (assert (<= 4 n))
  
  (let ((a1 0)
        (a2 0)
        (b1 0)
        (b2 0))
    (declare (type (unsigned-byte 64) a1 a2 b1 b2)
             (type (function (t t)) f)
             (dynamic-extent a1 a2 b1 b2))
    (labels
        ((all ()
           #+debug
           (format t "ALL: ~A ~D ~D ~D ~D~%" n a1 a2 b1 b2)
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
                       
                       ;; We avoid using WITH-REVERTED-OPERATIONS here for
                       ;; efficiency reasons. If we use that macro, then
                       ;; some of the reverted operations are then
                       ;; re-applied needlessly. We coalesce those here.
                       (progn
                         (setf a1 (logior a1 b1)
                               a2 (logior a2 b2))
                         (all)
                         
                         (setf a1 (logior a1 b2)
                               a2 (logior a2 b1))
                         (all)
                         (setf a1 (logxor a1 b1)
                               a2 (logxor a2 b2))

                         (all)
                         (setf a1 (logxor a1 b2)
                               a2 (logxor a2 b1)))
                       #+#:equivalent
                       (progn
                         (with-reverted-operations ((a1 (logior a1 b1))
                                                    (a2 (logior a2 b2)))
                           (all))

                         (with-reverted-operations ((a1 (logior a1 b1 b2))
                                                    (a2 (logior a2 b1 b2)))
                           (all))

                         (with-reverted-operations ((a1 (logior a1 b2))
                                                    (a2 (logior a2 b1)))
                           (all))))))))
         (greater ()
           #+debug
           (format t "GREATER: ~A ~D ~D ~D ~D~%" n a1 a2 b1 b2)
           (with-reverted-operations ((b1 (ash b1 1))
                                      (b2 (ash b2 -1)))
             (if (< n 4)
                 (with-reverted-operations ((a1 (logior a1 b1))
                                            (a2 (logior a2 b2))
                                            (n (- n 2)))
                   (all))
                 (with-reverted-operations ((n (- n 2)))
                   (greater)
                   
                   ;; We avoid using WITH-REVERTED-OPERATIONS here for
                   ;; efficiency reasons. If we use that macro, then
                   ;; some of the reverted operations are then
                   ;; re-applied needlessly. We coalesce those here.
                   (progn
                     (setf a1 (logior a1 b1)
                           a2 (logior a2 b2))
                     (all)
                     
                     (setf a1 (logior a1 b2)
                           a2 (logior a2 b1))
                     (greater)
                     
                     (setf a1 (logxor a1 b1 b2)
                           a2 (logxor a2 b1 b2)))
                   #+#:equivalent
                   (progn
                     (with-reverted-operations ((a1 (logior a1 b1))
                                                (a2 (logior a2 b2)))
                       (all))
                     (with-reverted-operations ((a1 (logior a1 b1 b2))
                                                (a2 (logior a2 b1 b2)))
                       (greater))))))))
      ;; Avoid repeated reverted subtraction-by-2.
      (decf n 2)
      
      ;; Prepare for first call to GREATER.
      (setf b1 1
            b2 (ash 1 (1+ n))
            a1 0
            a2 0)
      
      (greater)

      ;; Prepare for call to ALL.
      (setf b1 1
            b2 (ash 1 (1+ n))
            a1 b1
            a2 b2)
      
      (all)

      ;; Prepare for second call to GREATER.
      (setf b1 1
            b2 (ash 1 (1+ n))
            a1 (logior b1 b2)
            a2 a1)
      
      (greater)
      
      ;; Return nothing.
      (values))))
(declaim (notinline map-non-symmetric-bit-reversals-generic))

(defun print-bit-reversal (n)
  (flet ((print-pair (a b)
           (format t "~v,'0b   ~v,'0b~%" n a n b)))
    (map-non-symmetric-bit-reversals-generic n #'print-pair)))

(defun bit-reversed-permute! (x)
  "Permute the simple vector X of length 2^N in bit reversed order."
  (declare (type simple-vector x)
           (inline map-non-symmetric-bit-reversals-generic))
  (let* ((length (length x))
         (bits (integer-length (max 0 (1- length)))))
    ;; Check that this is a power of two.
    (assert (zerop (logand length (1- length))))
    (if (< bits 4)
        (bit-reversed-permute-naive! x)
        (flet ((swap (a b)
                 (rotatef (aref x a) (aref x b))))
          (declare (dynamic-extent #'swap))
          (map-non-symmetric-bit-reversals-generic bits #'swap)
          x))))


;;; Some tests
;;;
;;; This function doesn't even seem to work.
(declaim (inline reverse-bits))
(defun reverse-bits (value width)
  "Reverse the bits of the unsigned integer VALUE whose width is WIDTH bits."
  (declare (type (unsigned-byte 64) value)
           (type (integer 1 64) width))
  (loop :with r :of-type (unsigned-byte 64) := (logand value 1)
        :with s :of-type (integer 0 64) := (1- width)
        :for v :of-type (unsigned-byte 64) := (ash value -1) :then (ash v -1)
        :until (zerop v) :do
          (setf r (ash r 1))
          (setf r (logior r (logand v 1)))
          (decf s)
        :finally (return (the (unsigned-byte 64) (ash r s)))))
(declaim (notinline reverse-bits))

(defun map-non-symmetric-bit-reversals-generic-naive (n f)
  (declare (inline reverse-bits))
  (loop :for i :of-type (unsigned-byte 64) :below (expt 2 n)
        :for j :of-type (unsigned-byte 64) := (reverse-bits i n)
        :when (< i j) :do
          (funcall f i j))
  (values))

(defun print-bit-reversal-naive (n)
  (map-non-symmetric-bit-reversals-generic-naive
   n
   (lambda (a b)
     (format t "~v,'0B   ~v,'0B~%" n a n b))))

(defun bit-reversed-permute-naive! (x)
  "Permute the simple vector X of length 2^N in bit reversed order."
  (declare (type simple-vector x))
  (let* ((length (length x))
         (bits (integer-length (max 0 (1- length)))))
    ;; Check that this is a power of two.
    (assert (zerop (logand length (1- length))))
    (flet ((swap (a b)
             (rotatef (aref x a) (aref x b))))
      (declare (dynamic-extent #'swap))
      (map-non-symmetric-bit-reversals-generic-naive bits #'swap)
      x)))


;;; Comparison

(defun house-cleaning ()
  #+sbcl
  (sb-ext:gc :full t)
  #+lispworks
  (hcl:gc-all)
  #+ccl
  (ccl:gc))

(defun test-naive (n)
  (assert (<= 4 n 32))
  (let* ((l (expt 2 n))
         (x (make-array l :element-type 't
                          :initial-element 0)))
    (dotimes (i l) (setf (aref x i) i))
    (format t "Array size is ~D elements equaling about ~F KiB~%"
            l
            (/ (* l 4) 1024))
    (format t "Naive:~%")
    (dotimes (i 2)
      (house-cleaning)
      (time (bit-reversed-permute-naive! x)))
    (print x)
    (loop :for i :below l :always (= i (aref x i)))))

(defun test-strandh (n)
  (assert (<= 4 n 32))
  (let* ((l (expt 2 n))
         (x (make-array l :element-type 't
                          :initial-element 0)))
    (dotimes (i l) (setf (aref x i) i))
    (format t "Array size is ~D elements equaling about ~F KiB~%"
            l
            (/ (* l 4) 1024))
    (format t "Strandh-Elster:~%")
    (dotimes (i 2)
      (house-cleaning)
      (time (bit-reversed-permute! x)))
    (print x)
    (loop :for i :below l :always (= i (aref x i)))))

(defun compare-reversals (n &key (test-naive t) (test-strandh t))
  (assert (<= 4 n 32))
  (let* ((l (expt 2 n))
         (x (make-array l :element-type 't
                          :initial-element 0)))
    (dotimes (i l) (setf (aref x i) i))
    (format t "Array size is ~D elements equaling about ~F KiB~%"
            l
            (/ (* l 4) 1024))
    (when test-naive
      (format t "Naive:~%")
      (house-cleaning)
      (time (bit-reversed-permute-naive! x)))
    (when test-strandh
      (format t "Strandh-Elster:~%")
      (house-cleaning)
      (time (bit-reversed-permute! x)))
    (loop :for i :below l :always (= i (aref x i)))))
