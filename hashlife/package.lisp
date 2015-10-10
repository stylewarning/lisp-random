;;;; package.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

(defpackage #:hashlife
  (:use #:cl)
  (:export
   #:timestep                           ; FUNCTION
   #:hyperstep                          ; FUNCTION
   #:empty-macrocell                    ; FUNCTION
   #:pad-macrocell                      ; FUNCTION
   #:macrocell-cell                     ; FUNCTION
   #:macrocell-set-cell                 ; FUNCTION
   )
  )

