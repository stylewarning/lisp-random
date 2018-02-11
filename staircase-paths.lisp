;;;; staircase-paths.lisp
;;;;
;;;; Copyright (c) 2018 Robert Smith

;;; Problem defined in
;;;
;;;    http://www.watrophy.com/posts/37-Staircase-Paths.html

(defun normalization (z)
  (if (zerop z) 0 (/ z (abs z))))

(defun zero-vector (d)
  (make-array d :element-type 'fixnum :initial-element 0))

(defun map-staircase-paths (f p)
  "The set of all staircase paths from the origin to p. In the problem, this is denoted {0 -> p}. Apply the unary function F to all such paths.

What F calls are *not* lists ofcoordinates specifying a path, but rather a list of transitions (+1's) in the dimension specified starting at 0.
"
  (check-type p vector)
  (assert (notany #'minusp p))
  (let* ((d (length p)))
    (labels ((search-em-out (current-path where)
               (cond
                 ((some #'> where p)    ; Invalid path.
                  nil)
                 ((every #'= where p)   ; Finished path.
                  (funcall f current-path))
                 (t                     ; Try every step.
                  (dotimes (coord d)
                    (let ((where-to (copy-seq where)))
                      (incf (aref where-to coord))
                      ;; N.B., while we are seemingly accumulating in
                      ;; reverse order, the symmetry of the problem
                      ;; actually says that for every path Σ, then
                      ;; reverse(Σ) is also a valid path.
                      (search-em-out (cons coord current-path) where-to)))))))
      (search-em-out nil (zero-vector d)))))

(defun path-product (matrix path)
  (labels ((inc (coordinate i)
             (incf (nth i coordinate))
             coordinate)
           (rec (path coordinate product)
             (if (null path)
                 product
                 (rec (rest path)
                      (inc coordinate (first path))
                      (* product (apply #'aref matrix coordinate))))))
    (rec path (list 0 0) (aref matrix 0 0))))

(defun f-naive (matrix)
  (let ((sum 0))
    (flet ((contribute-path (path) (incf sum (path-product matrix path))))
      (destructuring-bind (m n) (array-dimensions matrix)
        (map-staircase-paths #'contribute-path (vector (1- m) (1- n)))
        (normalization sum)))))

;;; TODO: Allow M ≠ N; reduce storage requirements by only saving the
;;; boundary.
(defun f-smarter (matrix)
  (destructuring-bind (m n) (array-dimensions matrix)
    (assert (= m n))
    (let ((partials (make-array (list m n) :initial-element nil)))
      ;; Base case.
      ;;    Upper-left Corner.
      (setf (aref partials 0 0) (aref matrix 0 0))
      ;;    First row.
      (loop :for c :from 1 :below m :do
        (setf (aref partials 0 c)
              (* (aref matrix 0 c)
                 (aref partials 0 (1- c)))))
      ;;    First column.
      (loop :for r :from 1 :below m :do
        (setf (aref partials r 0)
              (* (aref matrix r 0)
                 (aref partials (1- r) 0))))
      ;; General case.
      (flet ((corner (r c)
               (setf (aref partials r c)
                     (* (aref matrix r c)
                        (+ (aref partials (1- r) c)
                           (aref partials r (1- c)))))))
        (loop :for ℓ :from 1 :below m :do
          (corner ℓ ℓ)                  ; Upper-leftmost corner.
          (loop :for rc :from (1+ ℓ) :below m :do
            (corner ℓ rc)               ; Row
            (corner rc ℓ))))            ; Column
      (normalization (aref partials (1- m) (1- n))))))

(defun random-T-matrix (m n)
  (let ((X (make-array (list m n))))
    (dotimes (r m X)
      (dotimes (c n)
        (setf (aref X r c) (cis (random #.(* 2 pi))))))))

(defun face-off (n)
  (let ((matrix (random-t-matrix n n)))
    (list (time (f-naive matrix))
          (time (f-smarter matrix)))))

;; CL-USER> (face-off 15)
;; Evaluation took:
;;   217.219 seconds of real time
;;   217.129113 seconds of total run time (215.784809 user, 1.344304 system)
;;   [ Run times consist of 3.241 seconds GC time, and 213.889 seconds non-GC time. ]
;;   99.96% CPU
;;   608,187,920,690 processor cycles
;;   257,773,207,296 bytes consed

;; Evaluation took:
;;   0.000 seconds of real time
;;   0.000041 seconds of total run time (0.000040 user, 0.000001 system)
;;   100.00% CPU
;;   111,712 processor cycles
;;   97,280 bytes consed

;; (#C(0.523738730674995d0 -0.8518789479679287d0)
;;  #C(0.5237387306751585d0 -0.8518789479678281d0))
