;;;; mobius.lisp
;;;; All of this written by and copyright (c) 2012 Kasadkad (Brendan Pawlowski)
;;;;
;;;; Marginally improved by Robert Smith

(defun bump (P i)
  (if (null P)
      (list (list i))
      (let ((Q (copy-list P)))
        (if (<= (car (last (first Q))) i)
            (cons (append (first Q) (list i)) (rest Q))
            (let ((first-row (copy-list (first Q))) j)
              (dotimes (k (length first-row))
                (when (> (nth k first-row) i)
                  (setf j k)
                  (return)))
              (setf (nth j first-row) i)
              (cons first-row (bump (rest Q) (nth j (first Q)))))))))
  
(defun RSK (word)
  (let (P)
    (dolist (i word P)
      (setf P (bump P i)))))

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

(defun tableau-size (P)
  (loop :for r :in P :sum (length r)))

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

;;; Remove DEFPARAMETER, add a LET and a SPECIAL declaration.
(defun mobius (P n)
  (let ((*mu* (make-hash-table :test #'equal)))
    (declare (special *mu*))            ;Declare *MU* as a dynamic variable.
    (dolist (Q (sort (upper-order-ideal P n) #'tableau>))
      (setf (gethash Q *mu*) (mobius-value Q n)))
    
    (gethash P *mu*)))