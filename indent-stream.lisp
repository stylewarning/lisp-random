;;;; indent-stream.lisp
;;;;
;;;; Copyright (c) 2020 Robert Smith

;;; This file provides an "interceptor stream" that lets you intercept
;;; characters and modify them before they're sent to their actual
;;; destination.
;;;
;;; This is then used to indent streams at newlines.

(defpackage #:indent-stream
  (:use #:cl #:trivial-gray-streams)
  (:export #:intercepted-output-stream))

(in-package #:indent-stream)

(defclass intercepted-output-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((underlying-stream :initarg :underlying-stream
                      :reader underlying-stream
                      :documentation "The stream that the INTERCEPTED-OUTPUT-STREAM ultimately writes to.")
   (interceptor-function :initarg :interceptor-function
                         :reader interceptor-function
                         :documentation "A function mapping CHARACTER -> {CHARACTER, STRING} that transforms written characters to the specified string."))
  (:default-initargs :underlying-stream *standard-output*
                     :interceptor-function 'identity))

(defmethod trivial-gray-streams:stream-write-char ((stream intercepted-output-stream) (char character))
  (let ((out (funcall (interceptor-function stream) char))
        (under (underlying-stream stream)))
    (etypecase out
      (character (write-char out under))
      (string    (write-string out under)))))

(defmethod trivial-gray-streams:stream-finish-output ((stream intercepted-output-stream))
  (finish-output (underlying-stream stream)))

(defmethod trivial-gray-streams:stream-force-output ((stream intercepted-output-stream))
  (force-output (underlying-stream stream)))


;;; Indenting lines

(defun indenting-interceptor (char)
  (case char
    (#\Newline #.(format nil "~%    "))
    (otherwise char)))

(defun make-indenting-stream (stream)
  (make-instance 'intercepted-output-stream
                 :underlying-stream stream
                 :interceptor-function 'indenting-interceptor))

;;; Try it out!

;;; INDENT-STREAM> (test)
;;;
;;; S : 123
;;; 456
;;;     SI: 123
;;;     456
;;;         SII: 123
;;;         456
(defun test ()
  (let* ((s *standard-output*)
         (si (make-indenting-stream s))
         (sii (make-indenting-stream si)))
    (format s "~&S : 123~%456")
    (format si "~&SI: 123~%456")
    (format sii "~&SII: 123~%456")))

