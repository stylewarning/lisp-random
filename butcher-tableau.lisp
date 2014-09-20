;;;; butcher-tableau.lisp
;;;; Copyright (c) 2014 Robert Smith

(defvar *rk4* '((0                  )
                (1/2 1/2            )
                (1/2 0   1/2        )
                (1   0   0   1      )
                (nil 1/6 1/3 1/3 1/6))
  "The classical Runge-Kutta method.")

(defvar *3/8-rule* '((0                   )
                     (1/3 1/3             )
                     (2/3 1/3  1          )
                     (1   1   -1   1      )
                     (nil 1/8  3/8 3/8 1/8))
  "The 3/8-rule. (More accurate than the classical Runge-Kutta but slightly less efficient.")

(defvar *euler* '((0    )
                  (nil 1))
  "The forward Euler method.")

(defclass butcher-tableau ()
  ((stages :initarg :stages
           :accessor butcher-tableau-stages
           :documentation "The number of stages of the Runge-Kutta process.")
   (matrix :initarg :matrix
           :accessor butcher-tableau-matrix
           :documentation "The Runge-Kutta matrix.")
   (weights :initarg :weights
            :accessor butcher-tableau-weights
            :documentation "The weights of the Runge-Kutta process.")
   (nodes :initarg :nodes
          :accessor butcher-tableau-nodes
          :documentation "The nodes of the Runge-Kutta process."))
  (:documentation "Representation of a Butcher tableau, used to create Runge-Kutta methods for solving differential equations. The shape of a Butcher tableau for an explicit Runge-Kutta method is as in the following table.

    c0* |
    c1  | a21
    c2  | a31   a32
    ... | ...
    cs  | a(s1) a(s2) ... a(s,s-1)
    -----------------------------------
        | b1    b2    ... b(s-1)    bs

    * The value of c0 is always zero.

Here, the vector c represents the \"nodes\", the vector b represents the \"weights\", and the lower-triangular matrix a represents the \"Runge-Kutta matrix\". The number s is the number of \"stages\" that the tableau represents.

Implicit methods are the same except there is a full coefficient matrix as opposed to a lower-triangular matrix."))

(defun parse-tableau (tab)
  (let ((s  (1- (length tab))))
    (flet ((populate-table (entries)
             (loop :with a := (make-array (list s (1- s)) :initial-element nil)
                   :for i :from 1 :to s
                   :for row :in entries
                   :do (loop :for j :from 1 :to (1- s)
                             :for node :in row
                             :do (setf (aref a (1- i) (1- j)) node))
                   :finally (return a))))
      (make-instance 'butcher-tableau
                     :stages s
                     :weights (coerce (cdar (last tab)) 'vector)
                     :nodes (coerce (butlast (mapcar #'first tab)) 'vector)
                     :matrix (populate-table (mapcar #'cdr (butlast tab)))))))

(defun is-number-p (n x)
  "Is the object X equal to the number N?"
  (check-type n number)
  (and (numberp x)
       (= n x)))

(defun is-zero-p (x)
  "Is the object X equivalent to zero?"
  (and (numberp x)
       (zerop x)))

(defparameter *process-rationals* t
  "When generating Runge-Kutta methods, process rational numbers so that they become explicit multiplications and divisions when multiplied.")

;;; TODO: Add a case to check if an argument is a RATIO. If so, encode
;;; it as a multiplication and a division.
(defun times (a b)
  "Construct the product of A and B. Do some minor simplification if either A or B are zero or one, or if A and B are both literal numbers."
  (labels ((process-rational (r x)
             "Process the rational R = P/Q so as to produce (/ (* P X) Q)."
             `(/ ,(times (numerator r) x)
                 ,(denominator r))))
    (cond
      ((or (is-zero-p a)
           (is-zero-p b)) 0)
      ((is-number-p 1 a) b)
      ((is-number-p 1 b) a)
      ((and (realp a)
            (realp b))
       (* a b))
      ((typep a 'ratio) (process-rational a b))
      ((typep b 'ratio) (process-rational b a))
      (t `(* ,a ,b)))))

(defun plus (a b)
  "Construct the sum of A and B. Do some minor simplification if either A or B"
  (cond
    ((is-zero-p a) b)
    ((is-zero-p b) a)
    ((and (realp a)
          (realp b))
     (+ a b))
    (t `(+ ,a ,b))))

(defun k-name (n)
  "Produce a symbol starting with K followed by the integer N."
  (intern (format nil "K~D" n)))

(defun generate-k (l bt &key (tn 'tn)
                             (yn 'yn)
                             (h  'h)
                             (f  'f)
                             (use-funcall t))
  (let ((first-arg
          (plus tn (times (aref (butcher-tableau-nodes bt) (1- l)) h)))
        
        (second-arg
          (loop :for i :below l
                :for sum := yn :then (plus sum
                                           (times (aref (butcher-tableau-matrix bt)
                                                        (1- l)
                                                        (1- i))
                                                  (k-name i)))
                :finally (return sum))))
    (times h (if use-funcall
                 `(funcall ,f ,first-arg ,second-arg)
                 `(,f ,first-arg ,second-arg)))))

(defun generate-y (bt &key (tn 'tn)
                           (yn 'yn)
                           (h  'h)
                           (f  'f))
  (declare (ignore f h tn))
  (loop :for i :from 0 :to (butcher-tableau-stages bt)
        :for y := yn :then (plus y (times (aref (butcher-tableau-weights bt) (1- i))
                                          (k-name i)))
        :finally (return y)))

(defun generate-rk-method (bt &key (yn 'yn)
                                   (tn 'tn)
                                   (h 'h)
                                   (f 'f)
                                   (use-funcall t))
  "Given a Butcher tableau BT, and values (possibly symbols) for:

    1. The current approximation of Y: YN,
    2. The current time step TN,
    3. The temporal quantization H, and
    4. The function F of the differential equation

        dY/dt = F(t, Y),

generated code which will return two values: the next timestep T(N+1), and the approximation of Y at T(N+1).

If USE-FUNCALL is NIL, then F will be put in function call position as opposed to being FUNCALL'd."
  `(let* (,@(loop :for l :from 1 :to (butcher-tableau-stages bt)
                  :collect (list (k-name l)
                                 (generate-k l bt :tn tn
                                                  :yn yn
                                                  :h h
                                                  :f f
                                                  :use-funcall use-funcall))))
     (values
      (+ ,tn ,h)
      ,(generate-y bt :yn yn :tn tn :h h :f f))))

;;; Example: (pprint (generate-rk-method (parse-tableau *rk4*)))

(defmacro define-rk-iterator (name bt h)
  `(defun ,name (f steps y0 t0)
     (let ((yn y0)
           (tn t0))
       (dotimes (i steps)
         (multiple-value-setq (tn yn)
           ,(generate-rk-method bt :yn 'yn
                                   :tn 'tn
                                   :h h
                                   :f 'f)))
       (values yn tn))))
