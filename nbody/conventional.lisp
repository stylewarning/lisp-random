(declaim (optimize (speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0)))

(defconstant $solar-mass (* 4.0d0 3.141592653589793d0 3.141592653589793d0))
(defconstant $days-per-year 365.24d0)
(defconstant $dt 0.01d0)

(declaim (type (simple-array double-float (5))
               *x* *y* *z* *vx* *vy* *vz* *mass*))

(defparameter *x*
  (make-array
   5
   :element-type 'double-float
   :initial-contents '(0.0d0
                       4.84143144246472090d0
                       8.34336671824457987d0
                       1.28943695621391310d+01
                       1.53796971148509165d+01)))

(defparameter *y*
  (make-array
   5
   :element-type 'double-float
   :initial-contents '(0.0d0
                       -1.16032004402742839d0
                       4.12479856412430479d0
                       -1.51111514016986312d+01
                       -2.59193146099879641d+01)))

(defparameter *z*
  (make-array
   5
   :element-type 'double-float
   :initial-contents '(0.0d0
                       -1.03622044471123109d-01
                       -4.03523417114321381d-01
                       -2.23307578892655734d-01
                       1.79258772950371181d-01)))

(defparameter *vx*
  (make-array
   5
   :element-type 'double-float
   :initial-contents (list 0.0d0
                           (* 1.66007664274403694d-03 $days-per-year)
                           (* -2.76742510726862411d-03 $days-per-year)
                           (* 2.96460137564761618d-03 $days-per-year)
                           (* 2.68067772490389322d-03 $days-per-year))))

(defparameter *vy*
  (make-array
   5
   :element-type 'double-float
   :initial-contents (list 0.0d0
                           (* 7.69901118419740425d-03 $days-per-year)
                           (* 4.99852801234917238d-03 $days-per-year)
                           (* 2.37847173959480950d-03 $days-per-year)
                           (* 1.62824170038242295d-03 $days-per-year))))

(defparameter *vz*
  (make-array
   5
   :element-type 'double-float
   :initial-contents (list 0.0d0
                           (* -6.90460016972063023d-05 $days-per-year)
                           (* 2.30417297573763929d-05 $days-per-year)
                           (* -2.96589568540237556d-05 $days-per-year)
                           (* -9.51592254519715870d-05 $days-per-year))))

(defparameter *mass*
  (make-array
   5
   :element-type 'double-float
   :initial-contents (list $solar-mass
                           (* 9.54791938424326609d-04 $solar-mass)
                           (* 2.85885980666130812d-04 $solar-mass)
                           (* 4.36624404335156298d-05 $solar-mass)
                           (* 5.15138902046611451d-05 $solar-mass))))

(declaim (inline dot)
         (ftype (function (double-float double-float double-float)
                          (double-float 0.0d0))
                dot))
(defun dot (a b c)
  (+ (* a a) (* b b) (* c c)))

(declaim (ftype (function (fixnum) (values)) advance))
(defun advance (n-steps)
  (let ((x *x*)
        (y *y*)
        (z *z*)
        (vx *vx*)
        (vy *vy*)
        (vz *vz*)
        (mass *mass*))
    (declare (type (simple-array double-float (5))
                   x y z vx vy vz mass))
    (loop :repeat n-steps :do
      (loop :for i :below 5 :do
        (let ((xi (aref x i))
              (yi (aref y i))
              (zi (aref z i))
              (vxi (aref vx i))
              (vyi (aref vy i))
              (vzi (aref vz i))
              (mi (aref mass i)))
          (declare (type double-float xi yi zi vxi vyi vzi mi))
          (loop :for j :from (1+ i) :below 5 :do
            (let* ((dx (- xi (aref x j)))
                   (dy (- yi (aref y j)))
                   (dz (- zi (aref z j)))
                   (dsq (dot dx dy dz))
                   (mag (/ $dt (* dsq (sqrt dsq))))
                   (mj-mag (* (aref mass j) mag))
                   (mi-mag (* mi mag)))
              (declare (type double-float dx dy dz dsq mag mj-mag mi-mag))
              (decf vxi (* dx mj-mag))
              (decf vyi (* dy mj-mag))
              (decf vzi (* dz mj-mag))
              (incf (aref vx j) (* dx mi-mag))
              (incf (aref vy j) (* dy mi-mag))
              (incf (aref vz j) (* dz mi-mag))))
          (setf (aref vx i) vxi)
          (setf (aref vy i) vyi)
          (setf (aref vz i) vzi)))
      (loop :for i :below 5 :do
        (incf (aref x i) (* $dt (aref vx i)))
        (incf (aref y i) (* $dt (aref vy i)))
        (incf (aref z i) (* $dt (aref vz i))))))
  (values))

(defun energy ()
  (let ((x *x*)
        (y *y*)
        (z *z*)
        (vx *vx*)
        (vy *vy*)
        (vz *vz*)
        (mass *mass*)
        (e 0.0d0))
    (declare (type (simple-array double-float (5))
                   x y z vx vy vz mass)
             (type double-float e))
    (loop :for i :below 5 :do
      (incf e (* 0.5d0
                 (aref mass i)
                 (dot (aref vx i) (aref vy i) (aref vz i))))
      (loop :for j :from (1+ i) :below 5 :do
        (let* ((dx (- (aref x i) (aref x j)))
               (dy (- (aref y i) (aref y j)))
               (dz (- (aref z i) (aref z j))))
          (declare (type double-float dx dy dz))
          (decf e (/ (* (aref mass i) (aref mass j))
                     (sqrt (dot dx dy dz)))))))
    e))

(defun run-simulation (n-steps)
  (let ((start (get-internal-real-time)))
    (advance n-steps)
    (format t "~,9F~%" (energy))
    (format t "timing: ~D ms~%"
            (round (* 1000 (- (get-internal-real-time) start))
                   internal-time-units-per-second)))
  nil)

(defun main ()
  (let ((args sb-ext:*posix-argv*))
    (when (< (length args) 2)
      (format *error-output* "Usage: nbody <iterations>~%")
      (sb-ext:exit :code 1))
    (let ((n (parse-integer (second args))))
      (run-simulation n)
      (sb-ext:exit :code 0))))
