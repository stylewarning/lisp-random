;;;; bloom-filter.lisp
;;;; Copyright (c) 2012 Robert Smith

;;; The hashing scheme isn't quite correct. Our hash functions
;;; traditionally return a single index to set in the bit vector, but
;;; instead we just OR the hash -- which can have several bits -- into
;;; the bits. It is not evident if this is a correct formulation.

(defstruct (bloom-filter (:print-function (lambda (obj str depth)
                                            (declare (ignore depth))
                                            (print-unreadable-object
                                                (obj str :type t
                                                         :identity t)))))
  (count 0       :type (integer 0))
  (size 0        :type (integer 0))
  (bits 0        :type (integer 0))
  (hash-function
   (error "HASH-FUNCTION must be specified for a bloom filter.")
   :type (function (t) (integer 0))))

(defun bloom-filter-length (bf)
  (length (bloom-filter-bits bf)))

(defun bloom-filter-insert (bf object)
  (setf (bloom-filter-bits bf)
        (logior (bloom-filter-bits bf)
                (funcall (bloom-filter-hash-function bf) object)))
  (incf (bloom-filter-count bf)))

(declaim (inline log-contains-p))
(defun log-contains-p (a b)
  "Does the integer B contain all of the 1 bits that A has?"
  (zerop (logandc2 a b)))

(defun bloom-filter-add (bf object)
  "Add OBJECT to the Bloom filter BF."
  (setf (bloom-filter-bits bf)
        (logand (bloom-filter-bits bf)
                (funcall (bloom-filter-hash-function bf) object))))

(defun bloom-filter-contains-p (bf object)
  "Does the Bloom filter BF contain the object OBJECT?"
  (log-contains-p (funcall (bloom-filter-hash-function bf) object)
                  (bloom-filter-bits bf)))

(defun false-positive-probability (bf)
  "The probability that a query will lead to a false positive in the
Bloom filter BF."
  (let* ((n   (bloom-filter-count bf))
         (m=k (bloom-filter-size bf))
         (bit-prob (- 1.0d0 (/ m=k))))
    (expt (- 1.0d0 (expt bit-prob (* n m=k)))
          m=k)))
