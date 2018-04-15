;;;; qsim.lisp
;;;; Copyright (c) 2018 Robert Smith; see LICENSE for terms.

(defglobal +I+ (make-array '(2 2) :initial-contents '((1 0)
                                                      (0 1))))

(defglobal +X+ (make-array '(2 2) :initial-contents '((0 1)
                                                      (1 0))))

(defglobal +H+ (make-array '(2 2) :initial-contents (let ((s (/ (sqrt 2))))
                                                      (list (list s s)
                                                            (list s (- s))))))


(defglobal +SWAP+ (make-array '(4 4) :initial-contents '((1 0 0 0)
                                                         (0 0 1 0)
                                                         (0 1 0 0)
                                                         (0 0 0 1))))

(defglobal +CNOT+ (make-array '(4 4) :initial-contents '((1 0 0 0)
                                                         (0 1 0 0)
                                                         (0 0 0 1)
                                                         (0 0 1 0))))

(defun matrix-multiply (matrix column)
  (let* ((matrix-size (array-dimension matrix 0))
         (result (make-array matrix-size :initial-element 0.0d0)))
    (dotimes (i matrix-size)
      (let ((element 0))
        (dotimes (j matrix-size)
          (incf element (* (aref matrix i j) (aref column j))))
        (setf (aref result i) element)))

    (replace column result)))

(defun compose-operators (A B)
  (destructuring-bind (m n) (array-dimensions A)
    (let* ((l (array-dimension B 1))
           (result (make-array (list m l) :initial-element 0)))
      (loop :for i :below m :do
        (loop :for k :below l :do
          (loop :for j :below n :do
            (incf (aref result i k)
                  (* (aref A i j)
                     (aref B j k))))))
      result)))

(defun kronecker-multiply (A B)
  (destructuring-bind (m n) (array-dimensions A)
    (destructuring-bind (p q) (array-dimensions B)
      (let ((result (make-array (list (* m p) (* n q)))))
        (dotimes (i m result)
          (dotimes (j n)
            (let ((Aij (aref A i j))
                  (y (* i p))
                  (x (* j q)))
              (loop :for u :below p :do
                (loop :for v :below q :do
                  (setf (aref result (+ y u) (+ x v))
                        (* Aij (aref B u v))))))))))))

(defun kronecker-expt (U n)
  (cond
    ((< n 1) #2A((1)))
    ((= n 1) U)
    (t (kronecker-multiply (kronecker-expt U (1- n)) U))))

(defun quantum-state-qubits (state)
  (1- (integer-length (length state))))

(defun make-quantum-state (n)
  (let ((s (make-array (expt 2 n) :initial-element 0.0d0)))
    (setf (aref s 0) 1.0d0)
    s))

(defun quantum-operator-p (U)
  ;; doesn't check that U is unitary
  (and (= 2 (array-rank U))
       (= (array-dimension U 0) (array-dimension U 1))
       ;; Power of 2
       (zerop (logand (array-dimension U 0) (1- (array-dimension U 0))))))

(defun operator-qubits (U)
  (assert (quantum-operator-p U))
  (1- (integer-length (array-dimension U 0))))

(defun lift (U i n)
  (let ((left  (kronecker-expt +I+ (- n i (operator-qubits U))))
        (right (kronecker-expt +I+ i)))
    (kronecker-multiply left (kronecker-multiply U right))))

(defun apply-gate (state U qubits)
  (assert (= (length qubits) (operator-qubits U)))
  (if (= 1 (length qubits))
      (%apply-1Q-gate state U (first qubits))
      (%apply-nQ-gate state U qubits)))

(defun %apply-1Q-gate (state U q)
  (matrix-multiply (lift U q (quantum-state-qubits state))
                   state)
  state)

(defun permutation-to-transpositions (permutation)
  (let ((swaps nil))
    (dotimes (dest (length permutation) (nreverse swaps))
      (let ((src (elt permutation dest)))
        (loop :while (< src dest) :do
          (setf src (elt permutation src)))
        (cond
          ((< src dest) (push (cons src dest) swaps))
          ((> src dest) (push (cons dest src) swaps)))))))

(defun transpositions-to-adjacent-transpositions (transpositions)
  (flet ((expand-cons (c)
           (if (= 1 (- (cdr c) (car c)))
               (list (car c))
               (let ((trans (loop :for i :from (car c) :below (cdr c)
                                  :collect i)))
                 (append trans (reverse (butlast trans)))))))
    (mapcan #'expand-cons transpositions)))

(defun %apply-nQ-gate (state U qubits)
  (let ((n (quantum-state-qubits state)))
    (labels ((swap (i)
               (lift +swap+ i n))
             (transpositions-to-operator (trans)
               (reduce #'compose-operators trans :key #'swap)))
 
      (let* ((U01 (lift U 0 n))
             (from-space (append (reverse qubits)
                                 (loop :for i :below n
                                       :when (not (member i qubits))
                                         :collect i)))
             (trans (transpositions-to-adjacent-transpositions
                     (permutation-to-transpositions
                      from-space)))
             (to->from (transpositions-to-operator trans))
             (from->to (transpositions-to-operator (reverse trans)))
             (Upq (compose-operators to->from (compose-operators U01 from->to))))
        (matrix-multiply Upq state)
        state))))

(defun collapse (state basis-element)
  (fill state 0.0d0)
  (setf (aref state basis-element) 1.0d0))

(defun observe (state)
  (let ((r (random 1.0d0))
        (s 0.0d0))
    (dotimes (i (length state))
      (let ((prob_i (expt (abs (aref state i)) 2)))
        (incf s prob_i)
        (when (> s r)
          (collapse state i)
          (return-from observe (values state i)))))))

(defun run-quantum-program (qprog state)
  (loop :with n := (quantum-state-qubits state)
        :for (instruction . payload) :in qprog
        :do (ecase instruction
              ((gate)
               (destructuring-bind (gate &rest qubits) payload
                 (apply-gate state gate qubits)))
              ((measure)
               (let ((measurement (nth-value 1 (observe state))))
                 (format t "~&Measured: |~v,'0B>~&" n measurement))))
        :finally (return state)))

;;; example

(defun ghz-state (n)
  (cons `(gate ,+H+ 0)
        (loop :for q :below (1- n)
              :collect `(gate ,+CNOT+ ,q ,(1+ q)))))
