;;;; bitset.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

;;; Automatically-platform-optimized bit vectors, called "bitsets"
;;; since Common Lisp has the notion of a bit vector already.
;;;
;;; This code, while unsafe due to the DECLAIM, compiles to extremely
;;; efficient code on e.g. SBCL. On Mac OS X, 64-bit, the functions
;;; compile into code that is around 64 bytes long. Pretty decent, in
;;; my opinion.

;;; TODO:
;;;
;;;    * Add out-of-bounds checks.

(ql:quickload :alexandria)

(declaim (optimize speed (safety 0) (debug 0)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun type-length (type)
    "The number of bits we can hold in TYPE (when it represents a non-negative integer)."
    (cond
      ((eq 'cl:fixnum type)
       (if (zerop (logand most-positive-fixnum
                          (1+ most-positive-fixnum)))
           (integer-length most-positive-fixnum)
           (1- (integer-length most-positive-fixnum))))
      ((typep type '(cons (eql cl:unsigned-byte)))
       (second type))
      (t (error "Unknown type ~S" type))))
    
  (defun find-efficient-array-element-type ()
    "Find an efficient integer array element type that holds the most bits. If nothing could be found, return FIXNUM."
    ;; XXX: This could probably be improved if we want to broaden our
    ;; search to for example unsigned types.
    (let ((fixnum-available (eq 'cl:fixnum (upgraded-array-element-type 'cl:fixnum))))
      (loop :for i :from 1024 :downto (if fixnum-available ; Only search to FIXNUM bits.
                                          (1+ (type-length 'cl:fixnum))
                                          1)
            :for proposed-element-type := `(unsigned-byte ,i)
            :for upgraded-element-type := (upgraded-array-element-type proposed-element-type)
            :when (and (listp upgraded-element-type)
                       (eq 'cl:unsigned-byte (first upgraded-element-type)))
              :do (return upgraded-element-type)
            :finally (return 'cl:fixnum))))
  
  (alexandria:define-constant +efficient-array-element-type+ (find-efficient-array-element-type)
    :test 'equalp
    :documentation "A type which has an efficient representation in an array.")
  
  (defconstant +bit-count+ (type-length +efficient-array-element-type+)
    "The number of bits that can be held in a single array element.")

)                                       ; EVAL-WHEN

(deftype vector-index ()
  "A valid array index."
  `(integer 0 (#.array-total-size-limit)))

(deftype bit-position ()
  "A valid intra-word bit position."
  `(mod ,+bit-count+))

(deftype efficient-integer ()
  "An efficiently stored array element type."
  +efficient-array-element-type+)

(deftype bitset ()
  "Representation of a bit set."
  `(simple-array efficient-integer (*)))

(defconstant +ones+ (1- (ash 1 +bit-count+))
  "A total of +BIT-COUNT+ ones.")

(alexandria:define-constant +mask-table+
  (loop :with table := (make-array +bit-count+
                                   :element-type 'efficient-integer
                                   :initial-element 0)
        :for i :below +bit-count+
        :do (setf (aref table i) (ash 1 i))
        :finally (return table))
  :test 'equalp
  :documentation "Table storing masks for each bit position accessible within an EFFICIENT-INTEGER.")

(alexandria:define-constant +inverse-mask-table+
  (flet ((complement-mask (n)
           (logxor n (1- (expt 2 +bit-count+)))))
    (loop :with table := (make-array +bit-count+
                                     :element-type 'efficient-integer
                                     :initial-element 0)
          :for i :below +bit-count+
          :do (setf (aref table i) 
                    (complement-mask (aref table i)))
          :finally (return table)))
  :test 'equalp
  :documentation "Table storing complements of the masks for each bit position accessible within an EFFICIENT-INTEGER.")

(deftype bit-mask ()
  "The type of all EFFICIENT-INTEGER bit masks."
  `(integer 1 ,(1- (ash 1 +bit-count+))))

(declaim (ftype (function (bit-position) bit-mask) mask))
(declaim (inline mask))
(defun mask (bit-position)
  "A mask for bit position BIT-POSITION."
  (declare (type bit-position bit-position))
  #-sbcl
  (the bit-mask (aref +mask-table+ bit-position))
  #+sbcl
  (the bit-mask (ash 1 (the bit-position bit-position))))

(declaim (ftype (function (bit-position) bit-mask) inverse-mask))
(declaim (inline inverse-mask))
(defun inverse-mask (bit-position)
  "A complement of the mask for bit position BIT-POSITION."
  (declare (type bit-position bit-position))
  #-sbcl
  (the bit-mask (aref +inverse-mask-table+ bit-position))
  #+sbcl
  (ldb (byte +bit-count+ 0) (lognot (mask bit-position)))
  #+#:ignore
  (the bit-mask (logxor +ones+ (mask bit-position))))

(defmacro defun-inlinable (name args &body body)
  "Defines a function akin to DEFUN, but ensures it is able to be inlined."
  `(progn
     (declaim (inline ,name))
     (defun ,name ,args ,@body)
     (declaim (notinline ,name))))

(defun-inlinable bitset-length (bs)
  "The number of bits storable by the bitset BS."
  (declare (type bitset bs))
  (* +bit-count+ (length bs)))

(defun make-bitset (length-at-least)
  "Create a bitset whose length is sufficient to hold LENGTH-AT-LEAST bits. Return two values: the bitset and the actual length of the bitset."
  (check-type length-at-least (integer 0 #.array-total-size-limit))
  (let* ((bitset (make-array (ceiling length-at-least +bit-count+)
                             :element-type 'efficient-integer
                             :initial-element 0)))
    (values bitset (bitset-length bitset))))

(declaim (inline position-index/offset))
(defun position-index/offset (bit-position)
  "Given a bitset position BIT-POSITION, return two values: the position in the containing array and its bit offset."
  (declare (type vector-index bit-position))
  (floor bit-position +bit-count+))

(defun-inlinable bitset-set (bs n)
  "Destructively modify the bitset BS so that the bit indexed by N is 1 (on)."
  (declare (type bitset bs)
           (type vector-index n))
  (multiple-value-bind (index offset) (position-index/offset n)
    (declare (type bit-position offset))
    (setf (aref bs index)
          (logior (the efficient-integer (aref bs index))
                  (mask offset)))
    (values)))

(defun-inlinable bitset-clear (bs n)
  "Destructively modify the bitset BS so that the bit indexed by N is 0 (off)."
  (declare (type bitset bs)
           (type vector-index n))
  (multiple-value-bind (index offset) (position-index/offset n)
    (declare (type bit-position offset))
    (setf (aref bs index)
          (logand (the efficient-integer (aref bs index))
                  (inverse-mask offset)))
    (values)))

(defun-inlinable bitset-toggle (bs n)
  "Destructively modify the bitset BS so that the bit indexed by N is its complement."
  (declare (type bitset bs)
           (type vector-index n))
  (multiple-value-bind (index offset) (position-index/offset n)
    (declare (type bit-position offset))
    (setf (aref bs index)
          (logxor (the efficient-integer (aref bs index))
                  (mask offset)))
    (values)))

(declaim (ftype (function (bitset vector-index) bit) bitset-bit))
(defun-inlinable bitset-bit (bs n)
  "Extract the bit indexed N from the bitset BITSET.

This is an accessor function with SETF defined."
  (declare (type bitset bs)
           (type vector-index n))
  (multiple-value-bind (index offset) (position-index/offset n)
    (declare (type bit-position offset))
    (if (zerop (logand (the efficient-integer (aref bs index))
                       (mask offset)))
        0
        1)))

(defun (setf bitset-bit) (new-value bs n)
  (declare (inline bitset-set bitset-clear))
  (check-type new-value bit)
  (if (zerop new-value)
      (bitset-clear bs n)
      (bitset-set bs n)))

(defun pretty-print-bitset (bs &optional (stream *standard-output*))
  "Print out the bits contained within the bitset BS to the stream STREAM."
  (declare (type bitset bs))
  (loop :for i :below (bitset-length bs)
        :for bit :of-type bit := (bitset-bit bs i)
        :do (if (zerop bit)
                (write-char #\0 stream)
                (write-char #\1 stream))))
