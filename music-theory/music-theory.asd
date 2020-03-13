;;;; music-theory.asd

(asdf:defsystem #:music-theory
  :description "Elementary music theory calculations."
  :author "Robert Smith <robert@stylewarning.com>"
  :license  "Public Domain"
  :depends-on (#:coalton)
  :serial t
  :components ((:file "music-theory")))
