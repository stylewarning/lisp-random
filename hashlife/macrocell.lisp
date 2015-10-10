;;;; macrocell.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

(in-package #:hashlife)

;;; The macrocell data structure. Does not include hashing and caching.

(deftype hash-code ()
  `(and fixnum unsigned-byte))

;;; For the cached constructor function MAKE-MACROCELL, see the
;;; function in cache.lisp.
(defstruct (macrocell (:constructor %make-macrocell))
  ;; Level
  (level 0 :type unsigned-byte :read-only t)
  ;; Hash - Computed upon creation of the macrocell in
  ;; MAKE-MACROCELL. Depends solely on the children.
  (hash 0 :type hash-code :read-only t)
  ;; Corners - Each of these are either a bit when LEVEL = 1, or are
  ;; macrocells of level LEVEL - 1.
  (nw nil :type (or bit macrocell) :read-only t)
  (ne nil :type (or bit macrocell) :read-only t)
  (sw nil :type (or bit macrocell) :read-only t)
  (se nil :type (or bit macrocell) :read-only t)
  ;; RESULT - This is a macrocell of level LEVEL - 1 situated at the
  ;; center, which is the result of evolving 2^(LEVEL - 2)
  ;; generations.
  ;;
  ;; Leaf cells do not have a RESULT.
  (result nil :type (or null macrocell))
  ;; Next - This is a macrocell of level LEVEL - 1 situated at the
  ;; center, which is the result of evolving 1 generation.
  ;;
  ;; Leaf cells don't have a "next".
  (next nil :type (or null macrocell)))

(defun macrocell-width (mc)
  "Compute the \"physical\" width of the macrocell MC."
  (expt 2 (macrocell-level mc)))

(defun macrocell-leaf-p (mc)
  "Is the macrocell MC a leaf?"
  (= 1 (macrocell-level mc)))

;;; The corresponding "set" function, #'MACROCELL-SET-CELL, can be
;;; found in cache.lisp.
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
