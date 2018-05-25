;;;; mnist.lisp
;;;;
;;;; Copyright (c) 2018 Robert Smith

(defpackage #:mnist
  (:use #:cl)
  (:export #:*data-directory*           ; VARIABLE
           #:image                      ; TYPE
           #:image-id                   ; READER
           #:image-label                ; READER
           #:image-vector               ; READER
           #:load-images                ; FUNCTION
           #:load-training-images       ; FUNCTION
           #:load-test-images           ; FUNCTION
           ))

(in-package #:mnist)

(defparameter *data-directory* nil
  "The directory in which the standard-named MNIST data files are,
  uncompressed. This variable is only used for the functions
  LOAD-TRAINING-IMAGES and LOAD-TEST-IMAGES.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +images-magic-number+ 2051)
  (defconstant +labels-magic-number+ 2049)
  (defconstant +image-dimension+ 28)
  (defconstant +total-pixels+ (* +image-dimension+ +image-dimension+)))

(defstruct image
  "A structure containing MNIST image data. The ID is the one-based
index into the image file. The LABEL is an integer between 0 and 10
inclusive indicating which digit was written. The VECTOR is
a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (784)) of pixel values in row-major
order."
  (id nil     :read-only t :type fixnum)
  (label nil  :read-only t :type (integer 0 10))
  (vector nil :read-only t :type (simple-array (unsigned-byte 8) (#.+total-pixels+))))

(defmethod print-object ((obj image) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "id:~D/~D" (image-id obj) (image-label obj))))

(defun read-big-endian-ub32 (stream)
  (let ((b0 (read-byte stream))
        (b1 (read-byte stream))
        (b2 (read-byte stream))
        (b3 (read-byte stream)))
    (declare (type (unsigned-byte 8) b0 b1 b2 b3))
    (logior (ash b0 24)
            (ash b1 16)
            (ash b2  8)
            (ash b3  0))))

(defun read-ub32-ensuring-value (stream ensured-value)
  (let ((read-byte (read-big-endian-ub32 stream)))
    (assert (= ensured-value read-byte)
            ()
            "Read the byte ~D when I expected ~D."
            read-byte
            ensured-value)
    read-byte))

(defun read-image-labels (stream)
  (read-ub32-ensuring-value stream +labels-magic-number+)
  (let* ((n (read-big-endian-ub32 stream))
         (label-vector (make-array n)))
    (read-sequence label-vector stream)
    label-vector))

(defun read-image-vector (stream)
  (let ((a (make-array +total-pixels+ :element-type '(unsigned-byte 8))))
    (read-sequence a stream)
    a))

(defun read-image-vectors (stream)
  (read-ub32-ensuring-value stream +images-magic-number+)
  (let* ((n (read-big-endian-ub32 stream))
         (images (make-array n)))
    (read-ub32-ensuring-value stream 28)
    (read-ub32-ensuring-value stream 28)
    (dotimes (i n images)
      (setf (aref images i) (read-image-vector stream)))))

(defun load-images (images-path labels-path)
  "Given paths to the uncompressed images file IMAGES-PATH and the
uncompressed labels file LABELS-PATH, load the MNIST data and return a
vector of IMAGE objects."
  (let ((image-labels
          (with-open-file (s labels-path :direction ':input
                                         :element-type '(unsigned-byte 8))
            (read-image-labels s)))
        (image-vectors
          (with-open-file (s images-path :direction ':input
                                         :element-type '(unsigned-byte 8))
            (read-image-vectors s))))
    (assert (= (length image-labels)
               (length image-vectors))
            ()
            "Upon reading the images file ~A and the labels file ~A, ~
             I didn't find that they contained the same number of ~
             elements."
            images-path
            labels-path)
    ;; We are using MAP-INTO only to save a bit of space. :)
    (map-into image-vectors
              (let ((id 0))
                (lambda (label vector)
                  (make-image :id (incf id) :label label :vector vector)))
              image-labels
              image-vectors)))

(defun load-training-images ()
  "Load the training images into a vector of IMAGE objects. The directory *DATA-DIRECTORY* will be searched for the MNIST files

    training-images-idx3-ubyte
    training-labels-idx1-ubyte.
"
  (load-images (merge-pathnames "train-images-idx3-ubyte"
                                *data-directory*)
               (merge-pathnames "train-labels-idx1-ubyte"
                                *data-directory*)))

(defun load-test-images ()
  "Load the test images into a vector of IMAGE objects. The directory *DATA-DIRECTORY* will be searched for the MNIST files

    t10k-images-idx3-ubyte
    t10k-labels-idx1-ubyte.
"
  (load-images (merge-pathnames "t10k-images-idx3-ubyte"
                                *data-directory*)
               (merge-pathnames "t10k-labels-idx1-ubyte"
                                *data-directory*)))

