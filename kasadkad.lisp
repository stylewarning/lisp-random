;;;; Kasadkad's avoiding stuff
;;;; Copyright (c) 2012 Robert Smith

(declaim (optimize speed (safety 0) (debug 0)))

;;; UTILITY FUNCTIONS

(declaim (inline xor list-to-vector range iota))

(defun xor (a b)
  "Exclusive or on A and B."
  (and (or a b)
       (not (and a b))))

(defun list-to-vector (list)
  "Convert LIST into a vector."
  (declare (type list list))
  
  (make-array (length list) :initial-contents list))

(defun range (start end &optional (step 1))
  "Return the list of numbers n such that START <= n < END and
n = START + k*STEP."
  (declare (type fixnum start end step))
  (assert (<= start end))

  (loop :for i :from start :below end :by step :collecting i))

(defun iota (n)
  "Return [0, ..., N-1]."
  (declare (type fixnum n))
  (assert (not (minusp n)))

  (range 0 n))

;;; PERMUTATIONS

(defun order-isomorphic-p (a b)
  "Are A and B order-isomorphic?"
  (declare (type vector a b))
  
  (let ((n (length a)))
    (declare (type fixnum n))
    
    (when (= n (length b))
      (loop :for i :below n
            :always (loop :for j :from i :below n
                          :always (not (xor (<= (elt a i) (elt a j))
                                            (<= (elt b i) (elt b j)))))))))
(defun subsequences (x n)
  "Get all of the subsequences of X of length N."
  (declare (type fixnum n)
           (type vector x))
  
  (let ((lenx (length x)))
    (declare (type fixnum lenx))
    
    (cond
      ((or (> n lenx)
           (not (plusp n))) nil)
      ((= n lenx) (list x))
      (t (let ((total (1+ (- lenx n))))
           (loop :for i :below total
                 :collect (subseq x i (+ i n))))))))

(defun permutation-matches-p (perm pattern)
  "Does the permutation PERM have a subsequence which matches the
pattern PATTERN?"
  (declare (type vector perm pattern))
  
  (loop :for s :in (subsequences perm (length pattern))
        :thereis (order-isomorphic-p pattern s)))

(defun permutation-avoids-p (perm pattern)
  "Does the permutation PERM avoid the pattern PATTERN?"
  (declare (type vector perm pattern))
  
  (not (permutation-matches-p perm pattern)))

;;; This conses an awful lot.
(defun permutations (n)
  "Generate the elements of the permutation group S_N."
  (declare (type integer n))
  
  (labels ((perms (l)
             (declare (type list l))

             (if (null l)
                 (list nil)
                 (mapcan #'(lambda (x)
                             (mapcar #'(lambda (y) (cons x y))
                                     (perms (remove x l :count 1)))) l))))
    (mapcar #'list-to-vector (perms (iota n)))))

(defun avoiding-patterns (permutation pattern-size)
  "Compute a list of all of the patterns of size PATTERN-SIZE that
  avoid the permutation PERMUTATION."
  (declare (type (unsigned-byte 16) pattern-size))
  
  (let ((perms (permutations pattern-size)))
    (delete-if (lambda (pattern)
                 (permutation-matches-p permutation pattern))
               perms)))
