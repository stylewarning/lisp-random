(defpackage #:thom-asd
  (:use #:cl #:asdf))

(in-package #:thom-asd)

(defsystem thom
  :name "thom"
  :depends-on ("qtility" "lla")
  :serial t
  :components ((:file "thom")))