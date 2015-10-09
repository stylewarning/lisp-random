;;;; hashlife.asd
;;;;
;;;; Copyright (c) 2015 Robert Smith

(asdf:defsystem #:hashlife
  :description "An implementation of the Hashlife algorithm."
  :author "Robert Smith <quad@symbo1ics.com>"
  :license "BSD 3-clause (See LICENSE)"
  :serial t
  :components ((:file "package")
               (:file "macrocell")
               (:file "cache")
               (:file "display")
               (:file "timestep")
               (:file "hyperstep")
               (:file "hashlife")))
