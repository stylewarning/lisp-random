;;;; Kasadkad's avoiding stuff
;;;; Copyright (c) 2012 Robert Smith

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
(defun consecutive-subsequences (x n)
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

(defun subsequences (x m)
  (let ((combs nil))
    (labels ((comb1 (l c m)
               (when (>= (length l) m)
                 (if (zerop m) (return-from comb1 (push (coerce (reverse c) 'vector)
                                                        combs)))
                 (comb1 (cdr l) c m)
                 (comb1 (cdr l) (cons (first l) c) (1- m)))))
      (comb1 (coerce x 'list) nil m)
      combs)))

(defun scan-subsequences (f x m)
  (labels ((comb1 (l c m)
             (when (>= (length l) m)
               (when (zerop m)
                 (funcall f 
                          (coerce (reverse c) 'vector))
                 (return-from comb1))
               (comb1 (cdr l) c m)
               (comb1 (cdr l) (cons (first l) c) (1- m)))))
    (comb1 (coerce x 'list) nil m)))

(defun count-subsequences (x n)
  (let ((c 0))
    (scan-subsequences (lambda (s)
                         (declare (ignore s))
                         (incf c))
                       x
                       n)
    c))

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

(defun avoided-patterns (permutation pattern-size)
  "Compute a list of all of the patterns of size PATTERN-SIZE that are
  avoided by the permutation PERMUTATION."
  (declare (type (unsigned-byte 16) pattern-size))
  
  (let ((perms (permutations pattern-size)))
    (delete-if (lambda (pattern)
                 (permutation-matches-p permutation pattern))
               perms)))

;;; requires steinhaus-johnson-trotter . L I S P

(defun scan-avoided-patterns (f permutation pattern-size)
  (doperms (pattern pattern-size)
    (unless (permutation-matches-p permutation pattern)
      (funcall f pattern))))

(defun count-avoid (perm patlen)
  (let ((c 0))
    (scan-avoided-patterns (lambda (p)
                             (incf c))
                           perm
                           patlen)
    c))

(defun count-avoid-all (n k)
  "count all of the patterns that everything in S_n avoids where the
  patterns are in P_k"
  (let ((total 0))
    (doperms (perm n total)
      (incf total (count-avoid perm k)))))

(defun show-counts (max-n)
  (loop :for n :from 1 :to max-n
        :do (loop :for k :from 1 :to n
                  :do (format t "n=~A k=~A: ~A~%"
                              n
                              k
                              (count-avoid-all n k)))))
