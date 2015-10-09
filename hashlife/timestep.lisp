;;;; timestep.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

(in-package #:hashlife)

;;; Timestep macrocells.

(defun empty-macrocell (level)
  "Create an empty cell of level LEVEL."
  (assert (<= 0 level))
  (if (zerop level)
      0
      (let ((sub-empty (empty-macrocell (1- level))))
        (make-macrocell sub-empty sub-empty
                        sub-empty sub-empty))))

(defun pad-macrocell (mc)
  "Pad the macrocell MC with one additional level of empty cells."
  (let* ((level (macrocell-level mc))
         (empty (empty-macrocell (1- level))))
    (make-macrocell
     (make-macrocell empty empty
                     empty (macrocell-nw mc))
     (make-macrocell empty empty
                     (macrocell-ne mc) empty)
     (make-macrocell empty (macrocell-sw mc)
                     empty empty)
     (make-macrocell (macrocell-se mc) empty
                     empty empty))))

(defun pad-macrocell-until (mc level)
  "Pad MC so that its level is at least level."
  (if (< level (macrocell-level mc))
      mc
      (pad-macrocell-until (pad-macrocell mc) level)))

(defun center (mc)
  "Given a non-leaf macrocell MC, compute its center."0
  (assert (not (macrocell-leaf-p mc)) (mc) "Leaf macrocells don't have centers.")
  (let ((nw-se (macrocell-se (macrocell-nw mc)))
        (ne-sw (macrocell-sw (macrocell-ne mc)))
        (sw-ne (macrocell-ne (macrocell-sw mc)))
        (se-nw (macrocell-nw (macrocell-se mc))))
    (make-macrocell nw-se ne-sw
                    sw-ne se-nw)))

(defun sub-center (mc)
  "Given a macrocell MC, compute the center of its center."
  (center (center mc)))

(defun horizontal-center (w e)
  "Given two horizontally adjacent cells, compute the center of these.

    +----+----+        +--+====+--+
    |    |    |        |  |    |  |
    | W  | E  |  ===>  |  | ** |  |
    |    |    |        |  |    |  |
    +----+----+        +--+====+--+
"
  (let ((w-ne-se (macrocell-se (macrocell-ne w)))
        (e-nw-sw (macrocell-sw (macrocell-nw e)))
        (w-se-ne (macrocell-ne (macrocell-se w)))
        (e-sw-nw (macrocell-nw (macrocell-sw e))))
    (make-macrocell w-ne-se e-nw-sw
                    w-se-ne e-sw-nw)))

(defun vertical-center (n s)
  "Given two vertically adjacent cells, compute the center of these.

    +--+       +--+
    |N |       |  |
    |  |       +==+
    +--+  ===> |**|
    |  |       |  |
    |S |       +==+
    +--+       |  |
               +--+

Note that the height extension in the above diagram is just an artifact of using ASCII art."
  (let ((n-sw-se (macrocell-se (macrocell-sw n)))
        (n-se-sw (macrocell-sw (macrocell-se n)))
        (s-nw-ne (macrocell-ne (macrocell-nw s)))
        (s-ne-nw (macrocell-nw (macrocell-ne s))))
    (make-macrocell n-sw-se n-se-sw
                    s-nw-ne s-ne-nw)))

(defconstant +neighborhood-mask+ #b000011101010111
  "The bits of the neighborhood of a cell. The cell would be located at the 5th bit (zero indexed). See the documentation for #'TIMESTEP-CELL for more information.")

(defun timestep-cell (bits)
  "Given an integer BITS which whose MSB to LSB ordering is

    15 14 13 12
    11 10  9  8
     7  6  5  4
     3  2  1  0,

compute whether the cell location in `5' is on or off after an iteration."
  (let ((neighborhood-alives (logcount (logand +neighborhood-mask+ bits)))
        (alive? (logbitp 5 bits)))
    (if (or (= 3 neighborhood-alives)
            (and alive? (= 2 neighborhood-alives)))
        1
        0)))

(defun level-two-bits (mc)
  "Given a level-2 macrocell MC, give the bitwise representation of the macrocell. The bit ordering is described in the documentation of the function #'TIMESTEP-CELL."
  (assert (= 2 (macrocell-level mc)) (mc) "Can only get the bits of a cell at level 2.")
  (let ((bits 0))
    (loop :for y :from -2 :below 2 :do
      (loop :for x :from -2 :below 2 :do
        (setf bits (logior (ash bits 1) (macrocell-cell mc x y)))))
    ;; Return the bits.
    bits))

(defun timestep-base (mc)
  "Evolve a level-2 macrocell MC one timestep."
  (let ((bits (level-two-bits mc)))
    (make-macrocell (timestep-cell (ash bits -5)) (timestep-cell (ash bits -4))
                    (timestep-cell (ash bits -1)) (timestep-cell (ash bits  0)))))

(defun timestep (mc)
  "Given a macrocell MC, evolve it one timestep."
  (if (= 2 (macrocell-level mc))
      (timestep-base mc)
      (let ((n00 (center (macrocell-nw mc)))
            (n01 (horizontal-center (macrocell-nw mc)
                                    (macrocell-ne mc)))
            (n02 (center (macrocell-ne mc)))
            (n10 (vertical-center (macrocell-nw mc)
                                  (macrocell-sw mc)))
            (n11 (center (center mc)))
            (n12 (vertical-center (macrocell-ne mc)
                                  (macrocell-se mc)))
            (n20 (center (macrocell-sw mc)))
            (n21 (horizontal-center (macrocell-sw mc)
                                    (macrocell-se mc)))
            (n22 (center (macrocell-se mc))))
        (make-macrocell
         (timestep (make-macrocell n00 n01 n10 n11))
         (timestep (make-macrocell n01 n02 n11 n12))
         (timestep (make-macrocell n10 n11 n20 n21))
         (timestep (make-macrocell n11 n12 n21 n22))))))

(defun padded-timestep (mc)
  "Compute the next generation, ignoring cells not in the center of the result."
  (pad-macrocell (timestep mc)))
