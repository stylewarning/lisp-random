;;;; mnist.lisp
;;;;
;;;; Copyright (c) 2018 Robert Smith

;;;; This file contains a parser for MNIST data.
;;;;
;;;; This doesn't actually include the data. You'll need to download
;;;; that from
;;;;
;;;;     http://yann.lecun.com/exdb/mnist/
;;;;
;;;; as well as extract the gzip-compressed files with a utility like
;;;; gunzip. In the end, you should have four files:
;;;;
;;;;     t10k-images-idx3-ubyte
;;;;     t10k-labels-idx1-ubyte
;;;;     train-images-idx3-ubyte
;;;;     train-labels-idx1-ubyte
;;;;
;;;; You can how pass pathnames to the function LOAD-IMAGES to get
;;;; these. Or, for potentially more friendly usage, you can set the
;;;; variable MNIST:*DATA-DIRECTORY* and use the argumentless
;;;; functions LOAD-TRAINING-IMAGES and LOAD-TEST-IMAGES.
;;;;
;;;; The data is stored in a very straightforward way. Look at the
;;;; IMAGE structure documentation for details.
;;;;
;;;; Example usage:
;;;;
;;;; CL-USER> (setf *print-length* 5)
;;;; 5
;;;;
;;;; CL-USER> (setf mnist:*DATA-DIRECTORY* "/Users/robert/Scratch/mnist/")
;;;; "/Users/robert/Scratch/mnist/"
;;;;
;;;; CL-USER> (time (mnist:load-training-images))
;;;; Evaluation took:
;;;;   0.040 seconds of real time
;;;;   0.040860 seconds of total run time (0.023280 user, 0.017580 system)
;;;;   102.50% CPU
;;;;   114,062,734 processor cycles
;;;;   50,860,704 bytes consed
;;;;
;;;; #(#<MNIST:IMAGE id:1/5 {1004BD28C3}> #<MNIST:IMAGE id:2/0 {1004BD28E3}>
;;;;   #<MNIST:IMAGE id:3/4 {1004BD2903}> #<MNIST:IMAGE id:4/1 {1004BD2923}>
;;;;   #<MNIST:IMAGE id:5/9 {1004BD2943}> ...)
;;;;
;;;; CL-USER> (defun partition (images)
;;;;            (let ((categorized (make-array 10 :initial-element nil)))
;;;;              (loop :for img :across images :do
;;;;                (push img (aref categorized (mnist:image-label img))))
;;;;              categorized))
;;;; PARTITION
;;;;
;;;; CL-USER> (partition (mnist:load-training-images))
;;;; #((#<MNIST:IMAGE id:59988/0 {100808F323}>
;;;;    #<MNIST:IMAGE id:59973/0 {100808F143}>
;;;;    #<MNIST:IMAGE id:59953/0 {100808EEC3}>
;;;;    #<MNIST:IMAGE id:59945/0 {100808EDC3}>
;;;;    #<MNIST:IMAGE id:59941/0 {100808ED43}> ...)
;;;;   (#<MNIST:IMAGE id:59995/1 {100808F403}>
;;;;    #<MNIST:IMAGE id:59985/1 {100808F2C3}>
;;;;    #<MNIST:IMAGE id:59980/1 {100808F223}>
;;;;    #<MNIST:IMAGE id:59966/1 {100808F063}>
;;;;    #<MNIST:IMAGE id:59959/1 {100808EF83}> ...)
;;;;   (#<MNIST:IMAGE id:59992/2 {100808F3A3}>
;;;;    #<MNIST:IMAGE id:59986/2 {100808F2E3}>
;;;;    #<MNIST:IMAGE id:59984/2 {100808F2A3}>
;;;;    #<MNIST:IMAGE id:59975/2 {100808F183}>
;;;;    #<MNIST:IMAGE id:59972/2 {100808F123}> ...)
;;;;   (#<MNIST:IMAGE id:59997/3 {100808F443}>
;;;;    #<MNIST:IMAGE id:59981/3 {100808F243}>
;;;;    #<MNIST:IMAGE id:59979/3 {100808F203}>
;;;;    #<MNIST:IMAGE id:59965/3 {100808F043}>
;;;;    #<MNIST:IMAGE id:59962/3 {100808EFE3}> ...)
;;;;   (#<MNIST:IMAGE id:59976/4 {100808F1A3}>
;;;;    #<MNIST:IMAGE id:59952/4 {100808EEA3}>
;;;;    #<MNIST:IMAGE id:59944/4 {100808EDA3}>
;;;;    #<MNIST:IMAGE id:59942/4 {100808ED63}>
;;;;    #<MNIST:IMAGE id:59934/4 {100808EC63}> ...)
;;;;   ...)

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
  (label nil  :read-only t :type (integer 0 (10)))
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
