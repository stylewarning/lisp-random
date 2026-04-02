(in-package :nsim)

(defconstant +solar-mass+ (* 4d0 pi pi))
(defconstant +days-per-year+ 365.24d0)
(defconstant +dt+ 0.01d0)

(define-kernel-shape body 5
  x y z vx vy vz mass)

(defparameter *system*
  (make-body-system
   (list :x 0d0
         :y 0d0
         :z 0d0
         :vx 0d0
         :vy 0d0
         :vz 0d0
         :mass +solar-mass+)
   (list :x 4.84143144246472090d+00
         :y -1.16032004402742839d+00
         :z -1.03622044471123109d-01
         :vx (* 1.66007664274403694d-03 +days-per-year+)
         :vy (* 7.69901118419740425d-03 +days-per-year+)
         :vz (* -6.90460016972063023d-05 +days-per-year+)
         :mass (* 9.54791938424326609d-04 +solar-mass+))
   (list :x 8.34336671824457987d+00
         :y 4.12479856412430479d+00
         :z -4.03523417114321381d-01
         :vx (* -2.76742510726862411d-03 +days-per-year+)
         :vy (* 4.99852801234917238d-03 +days-per-year+)
         :vz (* 2.30417297573763929d-05 +days-per-year+)
         :mass (* 2.85885980666130812d-04 +solar-mass+))
   (list :x 1.28943695621391310d+01
         :y -1.51111514016986312d+01
         :z -2.23307578892655734d-01
         :vx (* 2.96460137564761618d-03 +days-per-year+)
         :vy (* 2.37847173959480950d-03 +days-per-year+)
         :vz (* -2.96589568540237556d-05 +days-per-year+)
         :mass (* 4.36624404335156298d-05 +solar-mass+))
   (list :x 1.53796971148509165d+01
         :y -2.59193146099879641d+01
         :z 1.79258772950371181d-01
         :vx (* 2.68067772490389322d-03 +days-per-year+)
         :vy (* 1.62824170038242295d-03 +days-per-year+)
         :vz (* -9.51592254519715870d-05 +days-per-year+)
         :mass (* 5.15138902046611451d-05 +solar-mass+))))

(define-pairwise-kernel advance-forces (s body dt)
  (let* ((dx (- i.x j.x))
         (dy (- i.y j.y))
         (dz (- i.z j.z))
         (dsq (+ (+ (* dx dx) (* dy dy)) (* dz dz)))
         (mag (/ dt (* dsq (real-sqrt dsq)))))
    (declare (type double-float dx dy dz dsq mag))
    (let ((dm-j (* mag j.mass))
          (dm-i (* mag i.mass)))
      (declare (type double-float dm-j dm-i))
      (decf i.vx (* dx dm-j))
      (decf i.vy (* dy dm-j))
      (decf i.vz (* dz dm-j))
      (incf j.vx (* dx dm-i))
      (incf j.vy (* dy dm-i))
      (incf j.vz (* dz dm-i)))))

(define-self-kernel advance-positions (s body dt)
  (incf self.x (* dt self.vx))
  (incf self.y (* dt self.vy))
  (incf self.z (* dt self.vz)))

(define-reduction-kernel (energy e 0d0) (s body)
  (:self
   (+ e (* (* 0.5d0 self.mass)
           (+ (+ (* self.vx self.vx) (* self.vy self.vy))
              (* self.vz self.vz)))))
  (:pair
   (let* ((dx (- i.x j.x))
          (dy (- i.y j.y))
          (dz (- i.z j.z)))
     (declare (type double-float dx dy dz))
     (- e (/ (* i.mass j.mass)
             (real-sqrt (+ (+ (* dx dx) (* dy dy))
                           (* dz dz))))))))

(define-kernel-step run-simulation (system body n :params ((dt double-float)))
  (advance-forces dt)
  (advance-positions dt))

(defun main ()
  (when (< (length sb-ext:*posix-argv*) 2)
    (format *error-output* "Usage: nbody <iterations>~%")
    (sb-ext:exit :code 1))
  (let* ((n (parse-integer (second sb-ext:*posix-argv*)))
         (start (get-internal-real-time)))
    (run-simulation n *system* +dt+)
    (let* ((e (energy *system*))
           (ms (round (* 1000 (- (get-internal-real-time) start))
                      internal-time-units-per-second)))
      (format t "~,9F~%" e)
      (format t "timing: ~D ms~%" ms))
    (sb-ext:exit :code 0)))
