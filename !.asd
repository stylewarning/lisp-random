(asdf:defsystem #:!
  :description "Don't depend on this!"
  :author "Robert Smith <robert@stylewarning.com>"
  :license "BSD 3-clause (See LICENSE)"
  :depends-on (#:alexandria
               #:split-sequence)
  :serial t
  :components ((:file "package")
               (:file "plot-distribution")))
