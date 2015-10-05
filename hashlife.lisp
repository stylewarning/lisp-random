;;;; hashlife.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

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
  (result nil :type (or null macrocell))
  )

(defun macrocell-width (mc)
  "Compute the \"physical\" width of the macrocell MC."
  ;; Side length is 2^LEVEL.
  ;; Area is (2^LEVEL)^2 = 2^(2*LEVEL).
  (expt 2 (* 2 (macrocell-level mc))))

(defun macrocell-leaf-p (mc)
  "Is the macrocell MC a leaf?"
  (= 1 (macrocell-level mc)))

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
    (let ((*random-state* #.(make-random-state t)))
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

(defun macrocell-center (mc)
  "Given a non-leaf macrocell MC, compute its center."
  (assert (not (macrocell-leaf-p mc)) (mc) "Leaf macrocells don't have centers.")
  (let ((nw-se (macrocell-se (macrocell-nw mc)))
        (ne-sw (macrocell-sw (macrocell-ne mc)))
        (sw-ne (macrocell-ne (macrocell-sw mc)))
        (se-nw (macrocell-nw (macrocell-se mc))))
    (make-macrocell nw-se ne-sw
                    sw-ne se-nw)))

