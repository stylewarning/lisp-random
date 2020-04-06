;;;; A QUANTUM CONDITIONAL READER
;;;;
;;;; BY STYLEWARNING

;;; need to quickload :QVM

;;; In this file we construct a
;;;
;;;     #±FEATURE FORM
;;;
;;; reader macro which conditionally includes FORM depending on the
;;; |FEATURE> basis state of a wavefunction.
;;;
;;; "What wavefunction must we measure?" I hear you ask from the
;;; distance. Well, the Common Lisp designers were smart, but they
;;; only looked 50 years into the future, not the requisite 75
;;; required for quantum foresight. If Common Lisp were modernized,
;;; I'd make Yet Another Global Variable called *WAVEFUNCTION* which
;;; contains a global wavefunction expressed in the basis of
;;; *FEATURE-BASIS*.
;;;
;;; By default, we'll have an 8 qubit pure state corresponding to the
;;; basis listed. To my knowledge, this is the first characterization
;;; of an aspect of Common Lisp as a 256-dimensional complex Hilbert
;;; space.
;;;
;;; Include NIL in the feature basis to absolutely wreck most people's
;;; code.
(defvar *feature-basis* '(:ansi-cl :unix :windows :sbcl :ccl :32-bit :64-bit nil))

(defvar *wavefunction* (qvm:make-pure-state (length *feature-basis*)))

(defun feature-qubits ()
  (length *feature-basis*))

;;; Now we got a quantum state, and a basis in which that state is
;;; expressed (which also consequently defines the Hilbert subspace of
;;; the countably infinite dimensional Hilbert featture space). Now,
;;; assuming that we measured a quantum state, which gives us a
;;; bitstring, we need to evaluate that bitstring against our feature
;;; expression. What we do is interpret FEATURE-EXPR as a Boolean
;;; expression on feature eigenstates in the computational basis. In
;;; the case a feature isn't a part of our Hilbert substace, we
;;; interpret as a Boolean value deduced from *FEATURES* itself, as is
;;; standard in Common Lisp.

(defun analyze-bitstring (bitstring feature-expr)
  (etypecase feature-expr
    (symbol (let ((n (position feature-expr *feature-basis* :test #'string=)))
              (if (null n)
                  (find feature-expr *features*)
                  (= 1 (elt bitstring n)))))
    (cons (ecase (car feature-expr)
            (and (every (lambda (f) (analyze-bitstring bitstring f)) (rest feature-expr)))
            (or  (some (lambda (f) (analyze-bitstring bitstring f)) (rest feature-expr)))
            (not (not (analyze-bitstring bitstring (second feature-expr))))))))

;;; "But wtf, you defined a state, but you didn't define a unitary
;;; action on that state!!!"
;;;
;;; I hear you, my friend. The initial quantum state is such that we
;;; sit in the excited states of each feature eigenstate present in
;;; *FEATURES*. From there, we define the unitary action on the
;;; quantum state by perturbing the state depending on how much
;;; consing you're doing. The more you CONS and MAKE-*, the farther
;;; the state will evolve from the nominal state.
;;;
;;; Specifically, the amount of consing determines how "quantum" the
;;; feature expression is:
;;;
;;;     No consing => normal #+ expansion
;;;                => not very quantum... (aka boring)
;;;
;;;     More consing => Closer toward a Haar-random operator on the state
;;;                  => SUPER EFFING QUANTUM (aka AWESOME)
;;;
;;; Cons your heart out to make your program Quantum Ready (TM).
(defun analyze-consing (expr)
  (etypecase expr
    (symbol (cond
              ;; bonus points for OG consing
              ((string= "CONS" expr)                  2)
              ((eql 0 (search "MAKE-" (string expr))) 1)
              (t                                      0)))
    (atom   0)
    (cons   (+ (analyze-consing (car expr))
               (analyze-consing (cdr expr))))))

(defun random-qubit-pair ()
  (let* ((n (feature-qubits))
         (a (random n))
         (delta (1+ (random (1- n)))))
    (values a (mod (+ a delta) n))))

(defun consing-unitary-action (expr)
  (let ((consing (analyze-consing expr)))
    (quil::with-inst ()
      ;; Put us in the nominal *FEATURES* eigenstate and do some small
      ;; Euler perturbations by up to 1.6%
      (dotimes (q (length *feature-basis*))
        (when (find (nth q *feature-basis*) *features*)
          (quil::inst "X"  ()                q))
        (quil::inst "RX" (list (random 0.1)) q)
        (quil::inst "RZ" (list (random 0.1)) q)
        (quil::inst "RX" (list (random 0.1)) q))
      ;; Reward the user for consing.
      (loop :repeat consing :do
        (multiple-value-bind (p q) (random-qubit-pair)
          (quil::inst "RZ" (list (random (* 2 pi))) p)
          (quil::inst "CNOT" () p q)
          (quil::inst "RX" (list (random pi)) p))))))

;;; Now we are ready to roll. We bring ourselves to the ground state
;;; of the feature space, compute the consing action, simulate the
;;; quantum effects, and perform a full measurement. From there, we
;;; can determine, by way of analyzing our feature expression, whether
;;; we include the form or not.

(defun quantum-feature-reader (stream char subchar)
  (declare (ignore char subchar))       ; get out of here CHAR we
                                        ; don't need you

  (qvm:set-to-zero-state *wavefunction*) ; start fresh
  (let* ((feature-expr (read stream))
         (form (read stream))
         (action (consing-unitary-action form))
         (program (make-instance 'quil:parsed-program
                                 :executable-code (coerce action 'vector)))
         (qvm (make-instance 'qvm:pure-state-qvm :number-of-qubits (feature-qubits)
                                                 :state *wavefunction*)))
    (qvm:load-program qvm program :supersede-memory-subsystem t)
    (qvm:run qvm)
    ;; Measure the quantum state and analyze what we collapsed to.
    (if (analyze-bitstring (nth-value 1 (qvm:measure-all qvm)) feature-expr)
        form
        (values))))

(set-dispatch-macro-character #\# #\± 'quantum-feature-reader)

;;; Try (SETF QVM:*TRANSITION-VERBOSE* T) to see quantum states evolve.
