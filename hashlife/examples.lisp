;;;; hashlife/examples.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

(eval-when (:load-toplevel :execute)
  (initialize-leaves))

(defvar *block*
  (pad-macrocell (make-macrocell 1 1 1 1)))

(defvar *blinker*
  (make-macrocell (make-macrocell 0 0
                                  0 0)
                  (make-macrocell 0 0
                                  0 0)
                  (make-macrocell 0 1
                                  0 0)
                  (make-macrocell 1 1
                                  0 0)))
