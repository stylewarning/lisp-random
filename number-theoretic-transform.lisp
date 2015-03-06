;;;; number-theoretic-transform.lisp
;;;;
;;;; Copyright (c) 2014-2015 Robert Smith

(declaim (optimize (speed 0) (safety 3) (debug 3)))

;;; Bit Reversal
(load "strandh-elster-reversal.lisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun power-of-two-p (n)
  "Is N a power-of-two?"
  (zerop (logand n (1- n))))

(defun next-power-of-two (n)
  "Find the minimum K such that N <= 2^K."
  (if (power-of-two-p n)
      (1- (integer-length n))
      (integer-length n)))


;;;;;;;;;;;;;;;;;;;;;;;;; Modular Arithmetic ;;;;;;;;;;;;;;;;;;;;;;;;;

(defun m+ (a b m)
  "Compute A + B (mod M)."
  (mod (+ a b) m))

(defun m- (a b m)
  "Compute A - B (mod M)."
  (mod (- a b) m))

(declaim (inline split-byte))
(defun split-byte (x bits)
  "Split the non-negative integer X into two values X0 and X1 such that

    X = X1 << bits + X0."
  (values (ldb (byte bits 0) x)
          (ash x (- bits))))

(declaim (inline join-bytes))
(defun join-bytes (x0 x1 bits)
  "Join the bytes X0 and X1 into a value X such that

    X = X1 << bits + X0.

Ideally BITS is greater than the size of X0."
  (+ x0 (ash x1 bits)))

(defun fixed-width-add (a b width &aux (width/2 (ash width -1)))
  "Add the numbers A and B of width no more than WIDTH bits, using temporary values whose width do not exceed WIDTH bits.

Return two values S0 and S1 such that

    A+B = S0 + S1 << WIDTH."
  (multiple-value-bind (a0 a1) (split-byte a width/2) 
    (multiple-value-bind (b0 b1) (split-byte b width/2) 
      (multiple-value-bind (low carry) (split-byte (+ a0 b0) width/2) 
        (multiple-value-bind (high carry) (split-byte (+ carry a1 b1) width/2) 
          (values (join-bytes low high width/2) carry))))))

(defun fixed-width-multiply (a b width &aux (width/2 (ash width -1)))
  "Multiply the numbers A and B of width no more than WIDTH bits, using temporary values whose width do not wxceed WIDTH bits.

Return two values P0 and P1 such that

    A*B = P0 + P1 << WIDTH."
  ;; Split operands into half-width components.
  (multiple-value-bind (a0 a1) (split-byte a width/2)
    (multiple-value-bind (b0 b1) (split-byte b width/2)
      ;; Compute partial products. If W = 2^WIDTH and W' = W/2, then
      ;;
      ;;   A   = A0 + A1*W'
      ;;   B   = B0 + B1*W'
      ;;
      ;;   A*B = (A0 + A1*W')*(B0 + B1*W')
      ;;       = A0*B0 + (A0*B1 + A1*B0)*W' + A1*B1*W
      ;;
      ;; Each of these sub-A*B products are of width WIDTH, and are
      ;; broken into half-width components as above, except for the
      ;; product C3 = A1*B1.
      (multiple-value-bind (c0-lo c0-hi) (split-byte (* a0 b0) width/2)
        (multiple-value-bind (c1a-lo c1a-hi) (split-byte (* a0 b1) width/2)
          (multiple-value-bind (c1b-lo c1b-hi) (split-byte (* a1 b0) width/2)
            (let ((c3 (* a1 b1)))
              ;; Compute columns and carries as in longhand
              ;; multiplication. Each column tracks WIDTH/2 bits.
              ;;
              ;; Column 0   = C0-LO
              ;; Column 1   = C0-HI + C1A-LO + C1B-LO
              ;; Column 2,3 = C1A-HI + C1B-HI + C3 + COL1-CARRY
              (multiple-value-bind (col11 col1-carry1) (add c1a-lo c1b-lo width/2)
                (multiple-value-bind (col1 col1-carry2) (add col11 c0-hi width/2)
                  (let ((col1-carry (+ col1-carry1 col1-carry2)))
                    (values (join-bytes c0-lo col1 width/2)
                            (+ c1a-hi
                               c1b-hi
                               c3
                               col1-carry))))))))))))

(defun m* (a b m)
  "Compute A*B (mod M)."
  (mod (* a b) m))

(defun inv-mod (x m)
  "Compute X^-1 (mod M)."
  (labels ((egcd (x b a u)
             (if (zerop x)
                 (if (= 1 b)
                     (mod a m)
                     (error "~D is not invertible in Z/~DZ" x m)) ; X divides M
                 (multiple-value-bind (q r) (floor b x)
                   (egcd r x u (- a (* u q)))))))
    (egcd x m 0 1)))

(defun m/ (a b m)
  "Compute A / B = A * B^-1 (mod M)."
  (m* a (inv-mod b m) m))

(defun expt-mod (a n m)
  "Compute A ^ N (mod M) for integer N."
  (when (minusp n)
    (setf a (inv-mod a m)))
  
  (let ((result 1))
    (loop
      (when (oddp n)
        (setf result (m* result a m)))
      (setf n (floor n 2))
      (when (zerop n)
        (return-from expt-mod result))
      (setf a (m* a a m)))))



;;;;;;;;;;;;;;;;;;;;;; Primes and Factorization ;;;;;;;;;;;;;;;;;;;;;;

(defun factor-out (n f)
  "Return the values (A, B) such that N = A * F^B."
  (loop :with pow := 0
        :while (zerop (mod n f))
        :do (setf n (floor n f))
            (incf pow)
        :finally (return (values n pow))))

(defparameter *default-miller-rabin-count* 10
  "The default number of Miller-Rabin tests to perform.")

(defun primep (n &optional (k *default-miller-rabin-count*))
  "Is the positive integer N a prime?

This test uses the Miller-Rabin primality procedure. The positive integer K determines how many times this probabilistic test is run. Higher K implies a higher probability of correctness when PRIMEP returns T."
  (check-type n (integer 1))
  (check-type k (integer 1))
  (flet ((rand ()
           "Produce a random between 2 and N-2 inclusive."
           (+ 2 (random (- n 3)))))
    (cond ((= n 1)   nil)
          ((< n 4)     t)
          ((evenp n) nil)
          (t
           (multiple-value-bind (d s) (factor-out (- n 1) 2)
             (labels ((strong-liar? (a)
                        (let ((x (expt-mod a d n)))
                          (or (= x 1)
                              (loop :repeat s
                                    :for y := x :then (m* y y n)
                                      :thereis (= y (- n 1)))))))
               (declare (inline strong-liar?))
               (loop :repeat k
                     :always (strong-liar? (rand)))))))))

(defun next-prime (n)
  "Compute the next prime number after the positive integer N."
  ;; Try to be a little more clever by skipping evens.
  (let ((n (if (oddp n) n (1- n))))
    (loop :for i :from (+ n 2) :by 2
          :until (primep i)
          :finally (return i))))

(defun factorize (number)
  "Compute the prime factorization of NUMBER. Return a list of conses (Pi . Ni) such that Pi are prime, Ni are positive integers, and the product of all Pi^Ni equals NUMBER."
  (check-type number (integer 2))
  (if (primep number)
      (list (cons number 1))
      (let ((factorization nil))
        (loop :for f := 2 :then (next-prime f)
              :until (= 1 number)
              :do (multiple-value-bind (next pow)
                      (factor-out number f)
                    (unless (zerop pow)
                      (push (cons f pow) factorization)
                      (setf number next)))
              :finally (return (nreverse factorization))))))

(defun factorization-string (factorization)
  "Generate a pretty printed string of the factorization FACTORIZATION as produced by `FACTORIZE'."
  (flet ((format-factor (f stream)
             (if (= 1 (cdr f))
                 (format stream "~D" (car f))
                 (format stream "~D^~D" (car f) (cdr f)))))
    (with-output-to-string (s)
      (loop :for f := (pop factorization)
            :while f :do
              (format-factor f s)
              (when factorization           ; there are more factors
                (write-string " * " s))))))


;;;;;;;;;;;;;;;;;;;;;;;;; Finding NTT Moduli ;;;;;;;;;;;;;;;;;;;;;;;;;

;;; One key thing to compute number theoretic transforms is to compute
;;; suitable moduli. A "suitable modulus" M is a prime such that M =
;;; K*N + 1. For fast number theoretic transforms, N should be a power
;;; of 2, which will in turn be the bound on the length of our
;;; transform.
;;;
;;; Note that in reality, M does not need to be prime. We just get
;;; very nice properties from such a choice, e.g.
;;;
;;;    * Coprimality with the transform length (N must be invertible
;;;      for final normalization),
;;;
;;;    * Inversion for all elements < M in Z/MZ

(defun find-suitable-moduli (transform-length &key (count 1))
  "Return a list of COUNT number of suitable moduli for transforms of length TRANSFORM-LENGTH."
  (let* ((min-power (next-power-of-two transform-length))
         (min (expt 2 min-power))
         (moduli nil))
    (loop :for i :from 1
          :for p-1 := min :then (+ p-1 min)
          :for p := (1+ p-1)
          :while (plusp count)
          :do (when (primep p)
                (multiple-value-bind (a power) (factor-out (1- p) 2)
                  (declare (ignore a))
                  (when (>= power min-power)
                    (push p moduli)
                    (decf count))))
          :finally (return (nreverse moduli)))))

(defun modulus-factorization (modulus)
  "Pretty-print the factorization of the modulus MODULUS."
  (concatenate 'string "1 + " (factorization-string (factorize (1- modulus)))))

(defun print-suitable-moduli (transform-length count &optional (stream *standard-output*))
  "Print out COUNT suitable moduli for a desired transform length of TRANSFORM-LENGTH to the stream STREAM.

Specifically, the following will be printed:

    * In square brackets, the width of the word needed to represent the modulus and population count of the modulus (i.e., the smallest exponent of 2 such that respective power of 2 is greater than or equal to the modulus),

    * The modulus M, and

    * The printed representation of the factorization of M - 1."
  (let ((moduli (find-suitable-moduli transform-length :count count)))
    (dolist (modulus moduli)
      (format stream "[~D,~D] ~D = ~A~%"
              (next-power-of-two modulus)
              (logcount modulus)
              modulus
              (modulus-factorization modulus)))))


;;;;;;;;;;;;;;;;;;;;;; Finding Primitive Roots ;;;;;;;;;;;;;;;;;;;;;;;

;;; Once we have a suitable modulus, we need to find primitive roots
;;; of unity for that modulus.

(defun primitive-root-test-function (m)
  "Generate a unary function which tests if a value is a primitive root of the field Z/mZ. (This presumes that the modulus M forms a field.)"
  (let* ((m-1 (1- m))
         (prime-factors (mapcar #'car (factorize m-1)))
         (test-powers (mapcar (lambda (f) (floor m-1 f)) prime-factors)))
    (lambda (a)
      (loop :for power :in test-powers
            :never (= 1 (expt-mod a power m))))))

(defun find-primitive-root (m)
  "Find the smallest primitive M-th root of unity for the prime modulus M."
  (loop :with primitivep := (primitive-root-test-function m)
        :for p := 2 :then (next-prime p)
        ;; :when (>= m p) :do (return-from find-primitive-root nil)
        :until (funcall primitivep p)
        :finally (return p)))

(defun ordered-root-from-primitive-root (r n m)
  "Compute a root of order N from the primitive M-th root of unity R.

Note: N should divide M-1."
  (expt-mod r
            (floor (1- m) n)
            m))

(defun naive-primitive-root-p (w m)
  "Is the number W a primitive root in the (supposed) field of integers mod M?

The method to test is derived from the definition of a primitive root, and as such, is a very expensive computation."
  (let ((seen (make-array m :element-type 'bit :initial-element 0)))
    (loop :for i :below m
          :for x := w :then (m* w x m)
          :do (print x)
          :do (setf (bit seen x) 1)
          :finally (progn (setf (aref seen 0) 1)
                          (return (notany #'zerop seen))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;; EXAMPLE OUTPUT ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; > (print-suitable-moduli (expt 2 53) 15)
;; [58,3] 180143985094819841 = 1 + 2^55 * 5
;; [59,5] 459367161991790593 = 1 + 2^53 * 3 * 17
;; [60,7] 855683929200394241 = 1 + 2^53 * 5 * 19
;; [60,4] 882705526964617217 = 1 + 2^54 * 7^2
;; [60,5] 891712726219358209 = 1 + 2^53 * 3^2 * 11
;; [61,4] 1261007895663738881 = 1 + 2^55 * 5 * 7
;; [61,6] 1288029493427961857 = 1 + 2^53 * 11 * 13
;; [61,4] 1450159080013299713 = 1 + 2^53 * 7 * 23
;; [61,5] 1945555039024054273 = 1 + 2^56 * 3^3
;; [61,5] 2053641430080946177 = 1 + 2^55 * 3 * 19
;; [61,6] 2098677426354651137 = 1 + 2^53 * 233
;; [61,8] 2287828610704211969 = 1 + 2^54 * 127
;; [62,5] 2422936599525326849 = 1 + 2^53 * 269
;; [62,4] 2485986994308513793 = 1 + 2^55 * 3 * 23
;; [62,5] 2747195772696002561 = 1 + 2^53 * 5 * 61
;;
;; > (find-primitive-root 180143985094819841)
;; 11


;;;;;;;;;;;;;;;;;;;;; Number-Theoretic Transform ;;;;;;;;;;;;;;;;;;;;;

(defun ntt-forward-matrix (N m w)
  "Compute the NTT matrix of size N x N over Z/mZ using the primitive Mth root of unity W of order N."
  (let ((matrix (make-array (list N N) :initial-element 1)))
    (loop :for row :from 0 :below N :do
      (loop :for col :from 0 :below N 
            :for exponent := (* col row)
            :do (setf (aref matrix row col)
                      (expt-mod w exponent m)))
          :finally (return matrix))))

(defun ntt-reverse-matrix (N m w)
  "Compute the inverse NTT matrix of size N x N over Z/mZ using the primitive Mth root of unity W of order N.

This is just the conjugate-transpose of the NTT matrix, scaled by N."
  (labels ((conjugate-transpose (in)
             (let ((out (make-array (array-dimensions in))))
               (loop :for row :below (array-dimension out 0) :do
                 (loop :for col :below (array-dimension out 1) :do
                   (setf (aref out col row)
                         (m/ (inv-mod (aref in row col) m) N m))))
               out)))
    (conjugate-transpose (ntt-forward-matrix N m w))))

(defun matmul (A B modulus)
  "Multiply the matrices A and B over Z/mZ for modulus MODULUS."
  (let* ((m (car (array-dimensions A)))
         (n (cadr (array-dimensions A)))
         (l (cadr (array-dimensions B)))
         (C (make-array `(,m ,l) :initial-element 0)))
    (loop :for i :below m :do
      (loop :for k :below l :do
        (setf (aref C i k)
              (mod (loop :for j :below n
                         :sum (* (aref A i j)
                                 (aref B j k)))
                   modulus))))
    C))

(defun matvecmul (matrix v m)
  "Multiply the matrix MATRIX by the column vector V over Z/mZ."
  (let* ((N (length v))
         (result (copy-seq v)))
    (loop :for row :below N
          :do (setf (aref result row)
                    (loop :for col :below N
                          :for x := (aref matrix row col)
                          :for y := (aref v col)
                          :for x*y := (m* x y m)
                          :for s := x*y :then (m+ s x*y m)
                          :finally (return s)))
          :finally (return result))))

(defun test-matrix (v m w)
  "Tests inversion property of matrix method."
  (let* ((N (length v))
         (mat (ntt-forward-matrix N m w))
         (invmat (ntt-reverse-matrix N m w)))
    (equalp v (matvecmul invmat
                         (matvecmul mat v m)
                         m))))

(defun ntt-forward-direct (in m w)
  "Compute the NTT of the vector IN over Z/mZ using the primitive Mth root of unity W of order (LENGTH IN)."
  (let* ((N (length in))
         (out (make-array N :initial-element 0)))
    (loop :for k :below N
          :for w^k := (expt-mod w k m)
          :do (setf (aref out k)
                    (loop :for j :below N
                          :for w^jk := (expt-mod w^k j m)
                          :sum (m* w^jk (aref in j) m) :into s
                          :finally (return (mod s m))))
          :finally (return out))))

(defun ntt-reverse-direct (in m w)
  "Compute the inverse NTT of the vector IN over Z/mZ using the primitive Mth root of unity W of order (LENGTH IN)."
  (setf w (inv-mod w m))
  (let* ((N (length in))
         (out (make-array N :initial-element 0)))
    (loop :for k :below N
          :for w^k := (expt-mod w k m)
          :do (setf (aref out k)
                    (loop :for j :below N
                          :for w^jk := (expt-mod w^k j m)
                          :sum (* w^jk (aref in j)) :into s
                          :finally (return (m/ (mod s m) N m))))
          :finally (return out))))

(defun test-direct (v m w)
  "Tests inversion property of the direct NTT's."
  (equalp v
          (ntt-reverse-direct (ntt-forward-direct v m w)
                              m w)))

(defun compute-ntt-using-various-methods (v m w)
  "Compute the forward and reverse NTT of the vector V over Z/mZ using the primitive Mth root of unity W of order (LENGTH V)."
  (let ((N (length v)))
    (format t "Forward:~%")
    (format t "  matrix: ~A~%"   (matmul (ntt-forward-matrix N m w) v m))
    (format t "  direct: ~A~%"   (ntt-forward-direct v m w))
    (format t "  fast  : ~A~%~%" (ntt-forward (copy-seq v) :modulus m :primitive-root w))
    
    (format t "Reverse:~%")
    (format t "  matrix: ~A~%" (matmul (ntt-reverse-matrix N m w) v m))
    (format t "  direct: ~A~%" (ntt-reverse-direct v m w))
    (format t "  fast  : ~A~%" (ntt-reverse (copy-seq v) :modulus m :primitive-root w))
    nil))

(defun ntt-forward (a &key ((:modulus m) (first (find-suitable-moduli (length a))))
                           ((:primitive-root w) (ordered-root-from-primitive-root
                                                 (find-primitive-root m)
                                                 (length a)
                                                 m)))
  "Compute the forward number-theoretic transform of the array of integers A, with modulus MODULUS and primitive root PRIMITIVE-ROOT. If they are not provided, a suitable one will be computed.

The array must have a power-of-two length."
  (format t "m=#x~X (~D)    w=~D~%" m m w)

  (let* ((N  (length a))
         (ln (1- (integer-length N))))
    (loop :for lsubn :from ln :downto 2 :do
      (let* ((subn (ash 1 lsubn))
             (subn/2 (floor subn 2))
             (w^j 1))
        (loop :for j :below subn/2 :do
          (loop :for r :from 0 :to (- n subn) :by subn :do
            (let* ((r+j (+ r j))
                   (r+j+subn/2 (+ r+j subn/2))
                   (u (aref a r+j))
                   (v (aref a r+j+subn/2)))
              (setf (aref a r+j)        (m+ u v m)
                    (aref a r+j+subn/2) (m* w^j (m- u v m) m))))
          (setf w^j (m* w w^j m)))
        (setf w (m* w w m))))
    
    (loop :for r :below N :by 2 :do
      (psetf (aref a r)      (m+ (aref a r) (aref a (1+ r)) m)
             (aref a (1+ r)) (m- (aref a r) (aref a (1+ r)) m))))
  
  (bit-reversed-permute! a)
  
  a)

(defun ntt-reverse (a &key ((:modulus m) (first (find-suitable-moduli (length a))))
                           ((:primitive-root w) (ordered-root-from-primitive-root
                                                 (find-primitive-root m)
                                                 (length a)
                                                 m)))
  "Compute the inverse number-theoretic transform of the array of integers A, with modulus MODULUS and primitive root PRIMITIVE-ROOT. If they are not provided, a suitable one will be computed.

The array must have a power-of-two length."
  (format t "m=#x~X (~D)    w=~D~%" m m w)
  (setf w (inv-mod w m))
  (let* ((N   (length a))
         (ln (1- (integer-length N))))
    (loop :for lsubn :from ln :downto 2 :do
      (let* ((subn (ash 1 lsubn))
             (subn/2 (floor subn 2))
             (w^j 1))
        (loop :for j :below subn/2 :do
          (loop :for r :from 0 :to (- n subn) :by subn :do
            (let* ((r+j (+ r j))
                   (r+j+subn/2 (+ r+j subn/2))
                   (u (aref a r+j))
                   (v (aref a r+j+subn/2)))
              (setf (aref a r+j)        (m+ u v m)
                    (aref a r+j+subn/2) (m* w^j (m- u v m) m))))
          (setf w^j (m* w w^j m)))
        (setf w (m* w w m))))
    
    ;; This includes normalization.
    (loop :for r :below N :by 2 :do
      (psetf (aref a r)      (m/ (m+ (aref a r) (aref a (1+ r)) m) N m)
             (aref a (1+ r)) (m/ (m- (aref a r) (aref a (1+ r)) m) N m))))
  
  (bit-reversed-permute! a)
  
  a)

(defun test-ntt (v m w)
  "Tests inversion property of the fast NTTs."
  (equalp v
          (ntt-reverse (ntt-forward (copy-seq v)
                                    :modulus m
                                    :primitive-root w)
                       :modulus m
                       :primitive-root w)))


;;;;;;;;;;;;;;;;;;;; Reference DIF FFT algorithm ;;;;;;;;;;;;;;;;;;;;;

(defun dif-forward (a)
  "Compute the radix-2 decimation-in-frequency FFT of the complex vector A.

The vector must have a power-of-two length."
  (let* ((N   (length a))
         (ldn (1- (integer-length N))))
    (loop :for ldm :from ldn :downto 2 :do
      (let* ((m (ash 1 ldm))
             (m/2 (floor m 2)))
        (loop :for j :below m/2
              :for w^j := (cis (/ (* 2 pi j) m)) :do
                (loop :for r :from 0 :to (- n m) :by m :do
                  (let* ((r+j (+ r j))
                         (r+j+m/2 (+ r+j m/2))
                         (u (aref a r+j))
                         (v (aref a r+j+m/2)))
                    (setf (aref a r+j)     (+ u v)
                          (aref a r+j+m/2) (* w^j (- u v))))))))
    
    (loop :for r :below N :by 2 :do
      (psetf (aref a r)      (+ (aref a r) (aref a (1+ r)))
             (aref a (1+ r)) (- (aref a r) (aref a (1+ r))))))
  
  (bit-reversed-permute! a)
  
  a)

(defun dif-reverse (a)
  "Compute the radix-2 decimation-in-frequency inverse FFT of the complex vector A.

The vector must have a power-of-two length."
  (let* ((N   (length a))
         (ldn (1- (integer-length N))))
    (loop :for ldm :from ldn :downto 2 :do
      (let* ((m (ash 1 ldm))
             (m/2 (floor m 2)))
        (loop :for j :below m/2
              :for w^j := (cis (/ (* -2 pi j) m)) :do
                (loop :for r :from 0 :to (- n m) :by m :do
                  (let* ((r+j (+ r j))
                         (r+j+m/2 (+ r+j m/2))
                         (u (aref a r+j))
                         (v (aref a r+j+m/2)))
                    (setf (aref a r+j)     (+ u v)
                          (aref a r+j+m/2) (* w^j (- u v))))))))
    
    (loop :for r :below N :by 2 :do
      (psetf (aref a r)      (/ (+ (aref a r) (aref a (1+ r))) N)
             (aref a (1+ r)) (/ (- (aref a r) (aref a (1+ r))) N))))
  
  (bit-reversed-permute! a)
  
  a)

#+#:DOESNT-WORK
(defun dif-forward (f)
  (labels ((twiddle (k)
             (cis (/ (* -2 pi k) (length f))))
           (dif (start-e N)
             (unless (= N 1)
               (let* ((N/2 (floor N 2))
                      (start-o (+ start-e N/2)))
                 (loop :for k :below n/2
                       :for e+k := (+ start-e k)
                       :for o+k := (+ start-o k)
                       :for f_e+k := (aref f e+k)
                       :for f_o+k := (aref f o+k)
                       :for e := (+ f_e+k f_o+k)
                       :for o := (* (twiddle k)
                                    (- f_e+k f_o+k))
                       :do (setf (aref f e+k) e
                                 (aref f o+k) o))
                 
                 (dif start-e N/2)
                 (dif start-o N/2)))))
    ;; Perform the FTT via in-pace Decimation in Frequency.
    (dif 0 (length f))

    ;; Bit reverse
    (bit-reversed-permute! f)
    
    ;; Return the modified array.
    f))

#+#:DOESNT-WORK
(defun dit-forward (f)
  (labels ((twiddle (k)
             (cis (/ (* -2 pi k) (length f))))
           (dit (start-t N)
             (unless (= N 1)
               (let* ((N/2 (floor N 2))
                      (start-b (+ start-t N/2)))
                 (dit start-t N/2)
                 (dit start-b N/2)
                 (loop :for k :below n/2
                       :for t+k := (+ start-t k)
                       :for b+k := (+ start-b k)
                       :for top := (aref f t+k)
                       :for bot := (* (aref f b+k) (twiddle k))
                       :do (setf (aref f t+k) (+ top bot)
                                 (aref f b+k) (- top bot)))))))
    
    ;; Bit reverse
    (bit-reversed-permute! f)
    
    ;; Perform the FTT via in-pace Decimation in Frequency.
    (dit 0 (length f))
    
    ;; Return the modified array.
    f))


;;;;;;;;;;;;;;; Demonstration Multiplication Algorithm ;;;;;;;;;;;;;;;

(defun digit-count (n)
  "How many decimal digits does it take to write N?"
  (if (zerop n)
      1
      (let* ((approx   (ceiling (integer-length n) (log 10.0d0 2)))
             (exponent (expt 10 (1- approx))))
        (if (> exponent n)
            (1- approx)
            approx))))

(defun least-power-of-two->= (n)
  "What is the least power-of-two greater than or equal to N?"
  (if (power-of-two-p n)
      n
      (ash 1 (integer-length n))))

(defun digits (n &key (size (digit-count n)))
  "Make an array of the decimal digits of N."
  (loop :with v := (make-array size :initial-element 0)
        :for i :from 0
        :while (plusp n) :do
          (multiple-value-bind (div rem) (floor n 10)
            (setf (aref v i) rem)
            (setf n div))
        :finally (return v)))

(defun carry (v)
  "Perform carry propagation on the vector V."
  (loop :for i :below (1- (length v))
        :when (<= 10 (aref v i))
          :do (multiple-value-bind (div rem) (floor (aref v i) 10)
                (setf (aref v i)      rem)
                (incf (aref v (1+ i)) div))
        :finally (return v)))

(defun undigits (v)
  "Take an array of decimal digits V and create a number N."
  (loop :for x :across (carry v)
        :for b := 1 :then (* 10 b)
        :sum (* b x)))

(defun multiply (a b)
  "Multiply the integers A and B using NTT's."
  (let* ((a-count (digit-count a))
         (b-count (digit-count b))
         (length (* 2 (least-power-of-two->= (max a-count b-count))))
         (a-digits (digits a :size length))
         (b-digits (digits b :size length))
         (m (first (find-suitable-moduli (max length 101))))
         (w (ordered-root-from-primitive-root
             (find-primitive-root m)
             length
             m)))
    (format t "Multiplying ~D * ~D = ~D~%" a b (* a b))
    
    (format t "A's digits: ~A~%" a-digits)
    (format t "B's digits: ~A~%" b-digits)
    
    (setf a-digits (ntt-forward a-digits :modulus m :primitive-root w))
    (setf b-digits (ntt-forward b-digits :modulus m :primitive-root w))
    
    (format t "NTT(A): ~A~%" a-digits)
    (format t "NTT(B): ~A~%" b-digits)
    
    (setf a-digits (map-into a-digits (lambda (a b) (m* a b m)) a-digits b-digits))
    
    (format t "C = NTT(A)*NTT(B) mod ~D = ~A~%" m a-digits)
    
    (setf a-digits (ntt-reverse a-digits :modulus m :primitive-root w))
    
    (format t "NTT^-1(C): ~A~%" a-digits)
    
    (undigits a-digits)))

(defun fft-multiply (a b)
  "Multiply two non-negative integers A and B using FFTs."
  (let* ((a-count (digit-count a))
         (b-count (digit-count b))
         (length (* 2 (least-power-of-two->= (max a-count b-count))))
         (a-digits (digits a :size length))
         (b-digits (digits b :size length)))
    (format t "Multiplying ~D * ~D = ~D~%" a b (* a b))
    
    (format t "A's digits: ~A~%" a-digits)
    (format t "B's digits: ~A~%" b-digits)
    
    (setf a-digits (dif-forward a-digits))
    (setf b-digits (dif-forward b-digits))
    
    (format t "FFT(A): ~A~%" a-digits)
    (format t "FFT(B): ~A~%" b-digits)
    
    (setf a-digits (map-into a-digits (lambda (a b) (* a b)) a-digits b-digits))

    (format t "C = FFT(A)*FFT(B) = ~A~%" a-digits)
    
    (setf a-digits (dif-reverse a-digits))

    (format t "FFT^-1(C): ~A~%" a-digits)
    
    (undigits (map 'vector (lambda (z)
                             (round (realpart z)))
                   a-digits))))

;;;;;;;;;;;;;;;;;;;;; Chinese Remainder Theorem ;;;;;;;;;;;;;;;;;;;;;;

(defun chinese-remainder (congruence-equations)
  "Solves the set of relations

    x = a1  (mod m1)
    x = a2  (mod m2)
     ...
    x = aN  (mod mN)

for x. The congruences are encoded in the list of conses

    CONGRUENCES-EQUATIONS = ((a1 . m1) (a2 . m2) ... (aN . mN)).

All moduli must be pairwise coprime. Note that this requirement is satisfied by primes or powers of primes.

Returns two values: x and the composite modulus m1 * m2 * ... * mN."
  (loop :with composite-modulus := (reduce #'* congruence-equations :key #'cdr :initial-value 1)
        :for (a . m) :in congruence-equations
        :for complement-modulus := (floor composite-modulus m)
        :sum (* a
                (inv-mod complement-modulus m)
                complement-modulus)
          :into sum
        :finally (return (values (mod sum composite-modulus)
                                 composite-modulus))))
