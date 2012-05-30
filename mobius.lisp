;;;; mobius.lisp
;;;; All of this written by and copyright (c) 2012 Kasadkad (Brendan Pawlowski)
;;;;
;;;; Marginally improved by Robert Smith

(defun bump (P i)
  (if (null P)
      (list (list i))
      (if (<= (car (last (first P))) i)
          (cons (append (first P) (list i)) (rest P))
          (let* ((first-row (copy-list (first P)))
                 (j (position-if (lambda (x) (> x i)) first-row)))
            (setf (nth j first-row) i)
            (cons first-row (bump (rest P) (nth j (first P))))))))

;; This seems about 1 second faster for mobius 9 instead of
;; POSITION-IF.
;;
;; (loop :for k :below (length first-row)
;;       :for x :in first-row
;;       :while (<= x i)
;;       :finally (setf j k))


(defun RSK (word)
  (let (P)
    (dolist (i word P)
      (setf P (bump P i)))))

(declaim (inline iota+1))
(defun iota+1 (n)
  (loop :for i :from 1 :to n :collect i))

(defun covers (P n)
  (cond
    ((> (tableau-size P) n) nil)
    ((= (tableau-size P) n) (list (list (iota+1 (1+ n)))))
    (t (loop :with content := (apply 'concatenate 'list P)
             :for i :from 1 :to n
             :unless (member i content) ; This can be made O(1)
               :collect (bump P i)))))

(declaim (inline tableau-size))
(defun tableau-size (P)
  (loop :for r :in P :sum (length r)))

(declaim (inline tableau>))
(defun tableau> (P Q)
  (> (tableau-size P)
     (tableau-size Q)))

(defun strict-upper-order-ideal (P n)
  (if (> (tableau-size P) n)
      nil
      (reduce (lambda (X Y) (union X Y :test #'equal))
              (covers P n)
              :key (lambda (Q) (upper-order-ideal Q n)))))

(defun upper-order-ideal (P n)
  (append (strict-upper-order-ideal P n)
          (list P)))

(defun mobius-value (P n)
  (if (> (tableau-size P) n)
      1
      (- (reduce #'+
                 (strict-upper-order-ideal P n)
                 :key (lambda (Q)
                        (declare (special *mu*))
                        (gethash Q *mu*))))))

(defun mobius (P n)
  (let ((*mu* (make-hash-table :test #'equal)))
    (declare (special *mu*))
    (dolist (Q (sort (upper-order-ideal P n) #'tableau>))
      (setf (gethash Q *mu*) (mobius-value Q n)))
    
    (gethash P *mu*)))