(declaim (optimize speed (safety 0) (debug 0)))

(defconstant +factor+ 1)
(defparameter +width+ (* +factor+ 640))
(defparameter +height+ (* +factor+ 480))

(defun mandel (z0)
  (declare (type (complex double-float) z0))
  (do ((i 0 (1+ i))
       (z z0 (+ (* z z) z0)))
      ((or (= i 255) (> (abs z) 2)) i)
    (declare (type (integer 0 256) i)
             (type (complex double-float) z))
    ;; nothing, return i
    ))

(defun compute-mandel ()
  (with-open-file (f "result.pgm" :direction :output
                                  :if-exists :supersede
                                  :element-type '(unsigned-byte 8))
    (map nil 
         (lambda (c) (write-byte (char-code c) f))
         (format nil "P5~%~a ~a 255~%" +width+ +height+))

    (dotimes (y +height+)
      (declare (type (integer 0 640) y))
      (dotimes (x +width+)
        (declare (type (integer 0 480) y))
        (write-byte (mandel (complex (- (/ x (* +factor+ 240.0d0)) 2.d0)
                                     (- (/ y (* +factor+ 240.0d0)) 1.d0)))
                    f)))))
