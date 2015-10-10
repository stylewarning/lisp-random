;;;; charmlife.asd
;;;;
;;;; Copyright (c) 2015 Robert Smith

(asdf:defsystem #:charmlife
  :description "Hashlife running in the terminal."
  :author "Robert Smith <quad@symbo1ics.com>"
  :license "BSD 3-clause (See LICENSE)"
  :depends-on (#:hashlife #:cl-charms)
  :serial t
  :components ((:file "charmlife")))
