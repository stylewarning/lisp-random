;;;; median.lisp
;;;; Copyright (c) 2013 Robert Smith

;;;; PROBLEM
;;;;
;;;; Given M machines with an average of N B-bit integers per machine,
;;;; find the median.

;;;; SOLUTION
;;;;
;;;; In the following solution, a machine is represented simply by a
;;;; vector of integers (typically fixnums), and a collection of
;;;; machines is a list of vectors.
;;;;
;;;; The overall strategy is, instead of holistically computing the
;;;; median, we ask a machine if a particular number N is the median,
;;;; and it "responds" with yes or no, and if not, it gives us a hint
;;;; about what the median is relative to N (i.e., larger or
;;;; smaller). If we know the range of values the median can take (we
;;;; do, since it is a B-bit integer), this allows for a binary
;;;; search.
;;;;
;;;; When multiple machines are involved, instead of returning a
;;;; simple answer, we return a sort of distribution, describing the
;;;; relative position of a number in the data set. These
;;;; distributions can be combined with other machines' distributions,
;;;; and the final result can be tested to see if the distribution
;;;; represents a median.
;;;;
;;;; Unfortunately, as will be seen, there are lots of corner
;;;; cases. For example, the median of a data set with an even number
;;;; of elements is the average of the two middle numbers.


;;; Using a whole separate DIST struct is a little wasteful. We could
;;; just use 3-element vectors, so we can do MAP on them.
(defstruct dist
  "A simple tuple of numbers representing the relative distribution of
a number."
  (less-than    0 :type unsigned-byte)
  (equal-to     0 :type unsigned-byte)
  (greater-than 0 :type unsigned-byte))

(defun compute-single-dist (n machine)
  "Compute the distribution information about a number N of a single
machine MACHINE. The distribution information tells us how many
numbers in the machine are less than, equal to, or greater than the
number N."
  ;; In a functional language that's smart about immutable data
  ;; structures, this would be better represented as a fold.
  (let ((<n 0) (=n 0) (>n 0))
    (map nil
         (lambda (x)
           (cond
             ((< x n) (incf <n))
             ((> x n) (incf >n))
             (t       (incf =n))))
         machine)
    
    ;; Construct and return the distribution.
    (make-dist :less-than    <n
               :equal-to     =n
               :greater-than >n)))

(defun add-dists (d1 d2)
  "Add two distributions D1 and D2."
  (make-dist
   :less-than    (+ (dist-less-than d1) (dist-less-than d2))
   :equal-to     (+ (dist-equal-to d1) (dist-equal-to d2))
   :greater-than (+ (dist-greater-than d1) (dist-greater-than d2))))

(defun compute-dist (n machines)
  "Compute the distribution information of N on the list of machines
MACHINES."
  (let ((dist (reduce #'add-dists machines :key (lambda (d) 
                                                  (compute-single-dist n d)))))
    (values (dist-less-than    dist)
            (dist-equal-to     dist)
            (dist-greater-than dist))))

(defun compute-median (machines &key (min most-negative-fixnum)
                                     (max most-positive-fixnum))
  "Compute the median of the list of machines MACHINES, where the
minimum number on any machine is expressed by MIN and the maximum by
MAX."
  (labels ((half (x)
             (round x 2))
           
           (find-lub (n data)
             "Find the smallest number greater than N in the data set
              DATA."
             (loop :with lub = nil
                   :for x :across data
                   :do (when (and (or (null lub) (< x lub))
                                  (> x n))
                         (setf lub x))
                   :finally (return lub)))
           
           (find-glb (n data)
             "Find the largest number less than N in the data set
              DATA."
             (loop :with glb = nil
                   :for x :across data
                   :do (when (and (or (null glb) (> x glb))
                                  (< x n))
                         (setf glb x))
                   :finally (return glb)))
           
           (bisect (n min max)
             (format t "Trying N=~A in (~A, ~A)~%" n min max)
             ;; Have we closed our interval to [X, X+1]?
             (if (= (1+ min) max)
                 (/ (+ min max) 2)    ; Found median.
                 (multiple-value-bind (<n =n >n)
                     (compute-dist n machines)
                   (if (zerop =n)
                       ;; We did not find N in the data set, so we
                       ;; either need to balance out <N and >N by
                       ;; bisecting, or we are dead in the middle, and
                       ;; we need to find the numbers closest to N.
                       (cond
                         ((< <n >n) (bisect (half (+ n max)) n max))
                         ((> <n >n) (bisect (half (+ n min)) min n))
                         (t            
                          ;; This case sort of sucks. We are in the
                          ;; middle of the data set with our number N
                          ;; (which isn't actually a part of the
                          ;; data), and we need to find the two
                          ;; numbers P and Q such that P is the
                          ;; greatest number < N, and Q is the
                          ;; smallest number > N.
                          ;;
                          ;; We have to do this MAP+REDUCE shenanigans
                          ;; instead of a simple REDUCE+KEY since we
                          ;; need to remove null values possibly
                          ;; produced by FIND-* functions.
                          (let ((lub (reduce #'min
                                             (remove-if #'null
                                                        (mapcar (lambda (machine)
                                                                  (find-lub n machine))
                                                                machines))))
                                (glb (reduce #'max
                                             (remove-if #'null
                                                        (mapcar (lambda (machine)
                                                                  (find-glb n machine))
                                                                machines)))))
                            (/ (+ lub glb) 2)))) ; Found median.
                       
                       ;; We did find N in the data set. Suppose
                       ;; 
                       ;;     A = count of numbers < N,
                       ;;     B = count of numbers > N, and
                       ;;     C = count of numbers = N,
                       ;;     
                       ;; then we want A and B balanced up to C. In
                       ;; other words, we want
                       ;;
                       ;;     | A - B | < C.
                       ;;
                       ;; If they are balanced up to C, then N is the
                       ;; median. Otherwise, we rebalance by bisecting
                       ;; (which is done by checking if A - B is
                       ;; positive or negative, i.e., if A > B, vice
                       ;; versa.
                       (let ((delta (- <n >n)))
                         (cond
                           ((< (abs delta) =n) n)  ; Found median.
                           ((plusp delta) (bisect (half (+ n min)) min n))
                           (t             (bisect (half (+ n max)) n max)))))))))
    (bisect (half (+ max min)) min max)))

