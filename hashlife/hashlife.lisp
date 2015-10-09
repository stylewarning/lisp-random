;;;; hashlife/hashlife.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

;;; Currently requires SBCL.

(deftype hash-code ()
  `(and fixnum unsigned-byte))

(defstruct (macrocell (:constructor %make-macrocell))
  ;; Level
  (level 0 :type unsigned-byte :read-only t)
  ;; Hash - Computed upon creation of the macrocell in
  ;; MAKE-MACROCELL. Depends solely on the children.
  (hash 0 :type hash-code :read-only t)
  ;; Corners: Each of these are either a bit when LEVEL = 1, or are
  ;; macrocells of level LEVEL - 1.
  (nw nil :type (or bit macrocell) :read-only t)
  (ne nil :type (or bit macrocell) :read-only t)
  (sw nil :type (or bit macrocell) :read-only t)
  (se nil :type (or bit macrocell) :read-only t)
  ;; RESULT: This is a macrocell of level LEVEL - 1 situated at the
  ;; center, which is the result of evolving 2^(LEVEL - 2)
  ;; generations.
  ;;
  ;; Leaf cells do not have a RESULT.
  (result nil :type (or null macrocell)))

(defun macrocell-width (mc)
  "Compute the \"physical\" width of the macrocell MC."
  (expt 2 (macrocell-level mc)))

(defun macrocell-leaf-p (mc)
  "Is the macrocell MC a leaf?"
  (= 1 (macrocell-level mc)))

(defun macrocell-cell (mc x y)
  "Get the cell located at (X, Y) of the macrocell MC.

The coordinate system is defined to start at (-2^(LEVEL - 1), 2^(LEVEL - 1)) in the northwesternmost corner."
  (let ((level (macrocell-level mc)))
    (if (= 1 level)
        (cond
          ((and (= -1 x) (= -1 y))
           (macrocell-nw mc))
          ((and (= 0 x) (= -1 y))
           (macrocell-ne mc))
          ((and (= -1 x) (= 0 y))
           (macrocell-sw mc))
          ((and (= 0 x) (= 0 y))
           (macrocell-se mc))
          (t
           (error "Invalid coordinate: (~A, ~A)" x y)))
        (let ((offset (expt 2 (- level 2))))
          (if (minusp x)
              (if (minusp y)
                  (macrocell-cell (macrocell-nw mc) (+ x offset) (+ y offset))
                  (macrocell-cell (macrocell-sw mc) (+ x offset) (- y offset)))
              (if (minusp y)
                  (macrocell-cell (macrocell-ne mc) (- x offset) (+ y offset))
                  (macrocell-cell (macrocell-se mc) (- x offset) (- y offset))))))))

(defun combine-hashes (a b c d)
  "Given hash codes A, B, C, and D, combine them to create a new hash code."
  (declare (type hash-code a b c d))
  (let ((hash a))
    (declare (type hash-code hash))
    (sb-int:mixf hash b)
    (sb-int:mixf hash c)
    (sb-int:mixf hash d)
    hash))

(defun hash-from-children (nw ne sw se)
  "Construct a hash code form the macrocell children NW, NE, SW, and SE."
  (declare (type macrocell nw ne sw se))
  (combine-hashes (macrocell-hash nw)
                  (macrocell-hash ne)
                  (macrocell-hash sw)
                  (macrocell-hash se)))

(defun macrocell-equal (mc1 mc2)
  "Do the macrocells MC1 and MC2 represent the same state?"
  (declare (type macrocell mc1 mc2))
  (and (eq (macrocell-nw mc1) (macrocell-nw mc2))
       (eq (macrocell-ne mc1) (macrocell-ne mc2))
       (eq (macrocell-sw mc1) (macrocell-sw mc2))
       (eq (macrocell-se mc1) (macrocell-se mc2))))

(sb-ext:define-hash-table-test macrocell-equal macrocell-hash)

(declaim (type hash-table **macrocells**))
(sb-ext:defglobal **macrocells** (make-hash-table :test 'macrocell-equal
                                                  :weakness ':key-and-value)
    "EQ-identity map (mapping macrocells to macrocells) for caching.")

(declaim (type simple-vector **leaves**))
(sb-ext:defglobal **leaves** (make-array 16)
    "Simple vector containing the leaves. They are indexed by the bits they contaiin, namely, in MSB to LSB order, NW : NE : SW : SE.")

(defun initialize-leaves ()
  "Generate and store all of the initial leaf cells."
  (labels ((nth-bit (integer n)
             ;; Extract the Nth bit (from LSB to MSB) of the integer
             ;; INTEGER.
             (logand 1 (ash integer (- n))))
           (initialize-leaf (leaf-number)
             ;; LEAF-NUMBER is a 4-bit integer, where the LSB to MSB
             ;; represent the SE, SW, NE, and NW cells respectively.
             (let ((se (nth-bit leaf-number 0))
                   (sw (nth-bit leaf-number 1))
                   (ne (nth-bit leaf-number 2))
                   (nw (nth-bit leaf-number 3)))
               (setf (svref **leaves** leaf-number)
                     (%make-macrocell :level 1
                                      :hash (random most-positive-fixnum)
                                      :nw nw
                                      :ne ne
                                      :sw sw
                                      :se se)))))
    (unless (null (svref **leaves** 0)) ; Initialize once.
      (dotimes (i 16)
        (initialize-leaf i)))))

(defun make-macrocell (nw ne sw se)
  "Make a macrocell whose children are NW, NE, SW, and SE. These children may either all be macrocells of the same level, or all bits."
  (labels ((make-mc (nw ne sw se)
             (etypecase nw
               (bit
                (let ((index (logior (ash se 0)
                                     (ash sw 1)
                                     (ash ne 2)
                                     (ash nw 3))))
                  (return-from make-macrocell (svref **leaves** index))))
               (macrocell
                (assert (= (macrocell-level nw)
                           (macrocell-level ne)
                           (macrocell-level sw)
                           (macrocell-level se))
                        (nw ne sw se)
                        "Macrocells must be at the same level.")
                (%make-macrocell
                 :level (1+ (macrocell-level nw))
                 :hash (hash-from-children nw ne sw se)
                 :nw nw
                 :ne ne
                 :sw sw
                 :se se)))))
    (let* ((mc (make-mc nw ne sw se))
           (cached-mc (gethash mc **macrocells**)))
      (if (null cached-mc)
          (setf (gethash mc **macrocells**) mc)
          cached-mc))))

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

(defun center (mc)
  "Given a non-leaf macrocell MC, compute its center."0
  (assert (not (macrocell-leaf-p mc)) (mc) "Leaf macrocells don't have centers.")
  (let ((nw-se (macrocell-se (macrocell-nw mc)))
        (ne-sw (macrocell-sw (macrocell-ne mc)))
        (sw-ne (macrocell-ne (macrocell-sw mc)))
        (se-nw (macrocell-nw (macrocell-se mc))))
    (make-macrocell nw-se ne-sw
                    sw-ne se-nw)))

(defun center* (mc)
  "Given a non-leaf macrocell MC, compute the hyperstep of its center."
  (hyper-next-generation
   (make-macrocell
    (macrocell-se (macrocell-nw mc))
    (macrocell-sw (macrocell-ne mc))
    (macrocell-ne (macrocell-sw mc))
    (macrocell-nw (macrocell-se mc)))))

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

(defun horizontal-center* (w e)
  "Given two horizontally adjacent cells, compute the hyperstep of their center."
  (hyper-next-generation
   (make-macrocell
    (macrocell-ne w)
    (macrocell-nw e)
    (macrocell-se w)
    (macrocell-sw e))))

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

(defun vertical-center* (n s)
  "Given two vertically adjacent cells, compute the hyperstep of their center."
  (hyper-next-generation
   (make-macrocell
    (macrocell-sw n)
    (macrocell-se n)
    (macrocell-nw s)
    (macrocell-ne s))))

(defun show (mc &optional (stream *standard-output*))
  "Pretty print a macrocell MC to the stream STREAM, which is *STANDARD-OUTPUT* by default."
  (let* ((width (macrocell-width mc))
         (width/2 (/ width 2)))
    ;; Draw top border.
    (write-char #\+ stream)
    (loop :repeat width :do (write-char #\- stream))
    (write-char #\+ stream)
    (terpri stream)
    
    ;; Draw pattern.
    (loop :for y :from (- width/2) :below width/2 :do
      (write-char #\| stream)
      (loop :for x :from (- width/2) :below width/2 :do
        (if (= 1 (macrocell-cell mc x y))
            (write-char #\* stream)
            (write-char #\Space stream)))
      (write-char #\| stream)
      (terpri stream))
    
    ;; Draw top border.
    (write-char #\+ stream)
    (loop :repeat width :do (write-char #\- stream))
    (write-char #\+ stream)
    (terpri stream)))

(defconstant +neighborhood-mask+ #b000011101010111
  "The bits of the neighborhood of a cell. The cell would be located at the 5th bit (zero indexed). See the documentation for #'NEXT-GENERATION-CELL for more information.")

(defun next-generation-cell (bits)
  "Given an integer BITS which whose MSB to LSB ordering is

    15 14 13 12
    11 10  9  8
     7  6  5  4
     3  2  1  0,

compute whether the cell location in `5' is on or off."
  (let ((neighborhood-alives (logcount (logand +neighborhood-mask+ bits)))
        (alive? (logbitp 4 bits)))
    (if (or (= 3 neighborhood-alives)
            (and alive? (= 2 neighborhood-alives)))
        1
        0)))

(defun level-two-bits (mc)
  (let ((bits 0))
    (loop :for y :from -2 :below 2 :do
      (loop :for x :from -2 :below 2 :do
        (setf bits (logior (ash bits 1) (macrocell-cell mc x y)))))
    ;; Return the bits.
    bits))

(defun next-generation-base (mc)
  "Evolve a level-2 macrocell MC one timestep."
  (let ((bits (level-two-bits mc)))
    (make-macrocell (next-generation-cell (ash bits -5)) (next-generation-cell (ash bits -4))
                    (next-generation-cell (ash bits -1)) (next-generation-cell (ash bits  0)))))

(defun next-generation (mc)
  "Given a macrocell MC, evolve it one timestep."
  (if (= 2 (macrocell-level mc))
      (next-generation-base mc)
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
         (next-generation (make-macrocell n00 n01 n10 n11))
         (next-generation (make-macrocell n01 n02 n11 n12))
         (next-generation (make-macrocell n10 n11 n20 n21))
         (next-generation (make-macrocell n11 n12 n21 n22))))))

(defun padded-next-generation (mc)
  "Compute the next generation, ignoring cells not in the center of the result."
  (pad-macrocell (next-generation mc)))

(defun hyper-next-generation (mc)
  "Given a macrocell MC at level L, compute 2^(L-2) generations into the future."
  (labels ((compute-hyper (mc)
             (if (= 2 (macrocell-level mc))
                 (next-generation-base mc)
                 (hyper-general-case mc)))
           (hyper-general-case (mc)
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
          (setf (macrocell-result mc) (compute-hyper mc))
          result))))
