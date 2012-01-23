(defun xor (a b)
  "Exclusive or on A and B."
  (and (or a b)
       (not (and a b))))

(defun non-positive-p (n)
  "Is N non-positive?"
  (not (plusp n)))

(defun list-to-vector (list)
  "Convert LIST into a vector."
  (make-array (length list) :initial-contents list))

(defun range (start end &optional (step 1))
  "Return the list of numbers n such that START <= n < END and
n = START + k*STEP."
  (assert (<= start end))
  (loop :for i :from start :below end :by step :collecting i))

(defun iota (n)
  "Return [0, ..., N-1]."
  (assert (non-negative-p n))
  (range 0 n))

(defun order-isomorphic-p (a b)
  "Are A and B order-isomorphic?"
  (let ((n (length a)))
    (when (= n (length b))
      (loop :for i :below n
            :always (loop :for j :from i :below n
                          :always (not (qtl:xor (<= (elt a i) (elt a j))
                                                (<= (elt b i) (elt b j)))))))))
(defun subsequences (x n)
  "Get all of the subsequences of X of length N."
  (let ((lenx (length x)))
    (cond
      ((or (> n lenx)
           (qtl:non-positive-p n)) nil)
      ((= n lenx) (list x))
      (t (let ((total (1+ (- lenx n))))
           (loop :for i :below total
                 :collect (subseq x i (+ i n))))))))

(defun permutation-matches-p (perm pattern)
  "Does the permutation PERM have a subsequence which matches the
pattern PATTERN?"
  (loop :for s :in (subsequences perm (length pattern))
        :thereis (order-isomorphic-p pattern s)))

(defun permutation-avoids-p (perm pattern)
  "Does the permutation PERM avoid the pattern PATTERN?"
  (not (permutation-matches-p perm pattern)))

;;; This conses an awful lot.
(defun permutations (n)
  "Generate the elements of the permutation group S_N."
  (labels ((perms (l)
             (if (null l)
                 (list nil)
                 (mapcan #'(lambda (x)
                             (mapcar #'(lambda (y) (cons x y))
                                     (perms (remove x l :count 1)))) l))))
    (mapcar #'qtl:list-to-vector (perms (qtl:iota n)))))

(defun avoiding-patterns (permutation pattern-size)
  "Compute a list of all of the patterns of size PATTERN-SIZE that
  avoid the permutation PERMUTATION."
  (declare (type (unsigned-byte 16) pattern-size))
  (let ((perms (permutations pattern-size)))
    (delete-if (lambda (pattern)
                 (permutation-matches-p permutation pattern))
               perms)))
