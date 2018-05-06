;;;; qsim.lisp - a fully general quantum interpreter
;;;; Copyright (c) 2018 Robert Smith; see LICENSE for terms.

(defglobal +I+ (make-array '(2 2) :initial-contents '((1 0)
                                                      (0 1))))

(defglobal +SWAP+ (make-array '(4 4) :initial-contents '((1 0 0 0)
                                                         (0 0 1 0)
                                                         (0 1 0 0)
                                                         (0 0 0 1))))

(defun apply-operator (matrix column)
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
      (dotimes (i m result)
        (dotimes (k l)
          (dotimes (j n)
            (incf (aref result i k)
                  (* (aref A i j)
                     (aref B j k)))))))))

(defun kronecker-multiply (A B)
  (destructuring-bind (m n) (array-dimensions A)
    (destructuring-bind (p q) (array-dimensions B)
      (let ((result (make-array (list (* m p) (* n q)))))
        (dotimes (i m result)
          (dotimes (j n)
            (let ((Aij (aref A i j))
                  (y (* i p))
                  (x (* j q)))
              (dotimes (u p)
                (dotimes (v q)
                  (setf (aref result (+ y u) (+ x v))
                        (* Aij (aref B u v))))))))))))

(defun kronecker-expt (U n)
  (cond
    ((< n 1) #2A((1)))
    ((= n 1) U)
    (t (kronecker-multiply (kronecker-expt U (1- n)) U))))

(defstruct machine quantum-state measurement-register)

(defun dimension-qubits (d)
  (1- (integer-length d)))

(defun make-quantum-state (n)
  (let ((s (make-array (expt 2 n) :initial-element 0.0d0)))
    (setf (aref s 0) 1.0d0)
    s))

(defun lift (U i n)
  (let ((left  (kronecker-expt +I+ (- n i (dimension-qubits
                                           (array-dimension U 0)))))
        (right (kronecker-expt +I+ i)))
    (kronecker-multiply left (kronecker-multiply U right))))

(defun %apply-1Q-gate (state U q)
  (apply-operator (lift U q (dimension-qubits (length state)))
                  state))

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
  (let ((n (dimension-qubits (length state))))
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
             (Upq (compose-operators to->from
                                     (compose-operators U01
                                                        from->to))))
        (apply-operator Upq state)))))

(defun apply-gate (state U qubits)
  (assert (= (length qubits) (dimension-qubits (array-dimension U 0))))
  (if (= 1 (length qubits))
      (%apply-1Q-gate state U (first qubits))
      (%apply-nQ-gate state U qubits)))

(defun sample (state)
  (let ((r (random 1.0d0)))
    (dotimes (i (length state))
      (decf r (expt (abs (aref state i)) 2)) ; P(bitstring i) = |ψᵢ|²
      (when (minusp r) (return i)))))

(defun collapse (state basis-element)
  (fill state 0.0d0)
  (setf (aref state basis-element) 1.0d0))

(defun observe (machine)
  (let ((b (sample (machine-quantum-state machine))))
    (collapse (machine-quantum-state machine) b)
    (setf (machine-measurement-register machine) b)
    machine))

(defun run-quantum-program (qprog machine)
  (loop :for (instruction . payload) :in qprog
        :do (ecase instruction
              ((gate)
               (destructuring-bind (gate &rest qubits) payload
                 (apply-gate (machine-quantum-state machine) gate qubits)))
              ((measure)
               (observe machine)))
        :finally (return machine)))

;;; Example: A program to compute a Greenberger–Horne–Zeilinger state.

(defglobal +H+ (make-array '(2 2) :initial-contents (let ((s (/ (sqrt 2))))
                                                      (list (list s s)
                                                            (list s (- s))))))

(defglobal +CNOT+ (make-array '(4 4) :initial-contents '((1 0 0 0)
                                                         (0 1 0 0)
                                                         (0 0 0 1)
                                                         (0 0 1 0))))

(defun ghz (n)
  (cons `(gate ,+H+ 0)
        (loop :for q :below (1- n)
              :collect `(gate ,+CNOT+ ,q ,(1+ q)))))
