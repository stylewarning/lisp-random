;;;; sbcl-utilities.lisp
;;;;
;;;; Copyright (c) 2018 Robert Smith

(defpackage #:sbcl-utilities
  (:documetation "This package contains utilities that are hopelessly SBCL specific that are sometimes convenient.")
  (:use #:cl)
  (:export #:specific-type-of))

(defun specific-type-of (obj)
  "Determine the type of OBJ that's usually more specific than the
result of TYPE-OF."
  (sb-kernel:type-specifier (sb-kernel:ctype-of obj)))
