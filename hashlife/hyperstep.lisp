;;;; hyperstep.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

(in-package #:hashlife)

;;; "Hyperstep" macrocells, computing exponential numbers of timesteps.

(defun center* (mc)
  "Given a non-leaf macrocell MC, compute the hyperstep of its center."
  (hyperstep
   (make-macrocell
    (macrocell-se (macrocell-nw mc))
    (macrocell-sw (macrocell-ne mc))
    (macrocell-ne (macrocell-sw mc))
    (macrocell-nw (macrocell-se mc)))))

(defun horizontal-center* (w e)
  "Given two horizontally adjacent cells, compute the hyperstep of their center."
  (hyperstep
   (make-macrocell
    (macrocell-ne w)
    (macrocell-nw e)
    (macrocell-se w)
    (macrocell-sw e))))

(defun vertical-center* (n s)
  "Given two vertically adjacent cells, compute the hyperstep of their center."
  (hyperstep
   (make-macrocell
    (macrocell-sw n)
    (macrocell-se n)
    (macrocell-nw s)
    (macrocell-ne s))))

(defun hyperstep (mc)
  "Given a macrocell MC at level L, compute 2^(L-2) generations into the future."
  (labels ((compute-hyperstep (mc)
             (if (= 2 (macrocell-level mc))
                 (timestep-base mc)
                 (hyperstep-general-case mc)))
           (hyperstep-general-case (mc)
             (let ((n00 (hyper-next-generation (macrocell-nw mc)))
                   (n01 (horizontal-center* (macrocell-nw mc)
                                            (macrocell-ne mc)))
                   (n02 (hyper-next-generation (macrocell-ne mc)))
                   (n10 (vertical-center* (macrocell-nw mc)
                                          (macrocell-sw mc)))
                   (n11 (center* mc))
                   (n12 (vertical-center* (macrocell-ne mc)
                                          (macrocell-se mc)))
                   (n20 (hyper-next-generation (macrocell-sw mc)))
                   (n21 (horizontal-center* (macrocell-sw mc)
                                            (macrocell-se mc)))
                   (n22 (hyper-next-generation (macrocell-se mc))))
               (make-macrocell
                (hyper-next-generation (make-macrocell n00 n01 n10 n11))
                (hyper-next-generation (make-macrocell n01 n02 n11 n12))
                (hyper-next-generation (make-macrocell n10 n11 n20 n21))
                (hyper-next-generation (make-macrocell n11 n12 n21 n22))))))
    (let ((result (macrocell-result mc)))
      (if (null result)
          (setf (macrocell-result mc) (compute-hyperstep mc))
          result))))
