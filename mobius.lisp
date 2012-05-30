;;;; mobius.lisp
;;;; All of this written by and copyright (c) 2012 Kasadkad (Brendan Pawlowski)
;;;;
;;;; Improved efficiency by Robert Smith

(declaim (optimize speed (safety 0) (debug 0) (space 0)))

(deftype tableau-fixnum ()
  '(unsigned-byte 8))

(defun bump (P i)
  (declare (type tableau-fixnum i)
           (type list P))
  (if (null P)
      (list (list i))
      (if (<= (the tableau-fixnum (car (last (first P)))) i)
          (cons (append (first P) (list i)) (rest P))
          (let* ((first-row (copy-list (first P)))
                 (j (position-if (lambda (x)
                                   (declare (type tableau-fixnum x))
                                   (> x i)) 
                                 first-row)))
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
  (declare (type tableau-fixnum n))
  (loop :for i :from 1 :to n :collect i))

(declaim (inline tableau-size))
(defun tableau-size (P)
  (declare (values tableau-fixnum)
           (type list P))
  (loop :for r list :in P :sum (the tableau-fixnum (length r))))

(defun covers (P n)
  (declare (type tableau-fixnum n))
  (cond
    ((> (tableau-size P) n) nil)
    ((= (tableau-size P) n) (list (list (iota+1 (1+ n)))))
    (t (loop :with content := (apply 'concatenate 'list P)
             :for i :from 1 :to n
             :unless (member i content) ; This can be made O(1)
               :collect (bump P i)))))

(declaim (inline tableau>))
(defun tableau> (P Q)
  (> (tableau-size P)
     (tableau-size Q)))

(defun strict-upper-order-ideal (P n)
  (declare (type tableau-fixnum n))
  (if (> (tableau-size P) n)
      nil
      (reduce (lambda (X Y) (union X Y :test #'equal))
              (covers P n)
              :key (lambda (Q) (upper-order-ideal Q n)))))

(defun upper-order-ideal (P n)
  (declare (type fixnum n))
  (append (strict-upper-order-ideal P n)
          (list P)))

(defun mobius-value (P n)
  (declare (type tableau-fixnum n))
  (if (> (tableau-size P) n)
      1
      (- (reduce #'+
                 (strict-upper-order-ideal P n)
                 :key (lambda (Q)
                        (declare (special *mu*))
                        (gethash Q *mu*))))))

(defun mobius (P n)
  (declare (type tableau-fixnum n))
  (let ((*mu* (make-hash-table :test #'equal)))
    (declare (special *mu*))
    (dolist (Q (sort (upper-order-ideal P n) #'tableau>))
      (setf (gethash Q *mu*) (mobius-value Q n)))
    
    (gethash P *mu*)))

;;; END
;;; profile: (sb-profile:profile bump covers iota+1 tableau-size upper-order-ideal strict-upper-order-ideal mobius-value mobius)