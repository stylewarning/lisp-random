;;;; mobius.lisp
;;;; All of this written by and copyright (c) 2012 Kasadkad (Brendan Pawlowski)
;;;;
;;;; Marginally improved by Robert Smith


;;; change UNLESS ==> IF (NULL P)
;;; remove RETURN-FROM
;;; (append (list x) y) ==> (cons x y)
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
              (append (list first-row) (bump (rest Q) (nth j (first Q)))))))))
  
;;; change (dolist (a b) ...) P ==> (dolist (a b P) ...)
(defun RSK (word)
  (let (P)
    (dolist (i word P)
      (setf P (bump P i)))))

;;; change LISTP to ATOMP
;;; change UNLESS -> IF, and remove RETURN-FROM
;;; change (dolist (a b) ...) M ==> (dolist (a b M) ...)
#+#:ignore
(defun flatten (L)
  (if (atom L)
      L
      (let (M)
        (dolist (x L M)
          (setf M (append M (if (listp x)
                                (loop :for y :in (flatten x) :collect y)
                                (list x))))))))

;;; Better FLATTEN.
(defun flatten (L)
  (cond
    ((null L) nil)
    ((atom L) (list L))
    (t (mapcan #'flatten L))))

;;; Change to a COND.
;;; Change (let ((x foo)) (loop ...)) ==> (loop :with x := foo ...)
(defun covers (P n)
  (cond
    ((> (tableau-size P) n) nil)
    ((= (tableau-size P) n) (list (list (range 1 (+ 1 n)))))
    (t (loop :with content := (flatten P)
             :for i :from 1 :to n
             :unless (member i content) ; This can be made O(1)
               :collect (bump P i)))))

(defun range (a b)
  (loop :for i :from a :to b :collect i))

(defun tableau-size (P)
  (loop :for r :in P :sum (length r)))

;;; Change to IF.
;;; Change (reduce f (mapcar g L)) ==> (reduce f L :key g)
(defun upper-order-ideal (P n)
  (if (> (tableau-size P) n)
      (list P)
      (append 
       (reduce (lambda (X Y) (union X Y :test #'equal))
               (covers P n)
               :key (lambda (Q) (upper-order-ideal Q n)))
       (list P))))

(defun strict-upper-order-ideal (P n)
  (butlast (upper-order-ideal P n)))
  
;;; Change (reduce f (mapcar g L)) ==> (reduce f L :key g)
(defun mobius-value (P n)
  (if (> (tableau-size P) n)
      1
      (- (reduce #'+
                 (strict-upper-order-ideal P n)
                 :key (lambda (Q) (gethash Q *mu*))))))

;;; Remove DEFPARAMETER, add a LET and a SPECIAL declaration.
(defun mobius (P n)
  (let ((*mu* (make-hash-table :test #'equal)))
    (declare (special *mu*))            ;Declare *MU* as a dynamic variable.
    (dolist (Q (sort (upper-order-ideal P n)
                     (lambda (X Y) (> (tableau-size X) (tableau-size Y)))))
      (setf (gethash Q *mu*) (mobius-value Q n)))
    
    (gethash P *mu*)))