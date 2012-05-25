(defun linear-interpolator (p1 p2)
  "Construct a linear interpolator for the points

     P1=(X1 . Y1)

and

    P2 = (X2 . Y2)."
  (lambda (x)
    (let ((x1 (car p1)) (y1 (cdr p1))
          (x2 (car p2)) (y2 (cdr p2)))
      (let ((m (/ (- y2 y1)
                  (- x2 x1))))
        (+ y1 (* m (- x x1)))))))

(defun make-linear-interpolators (src)
  "Make a list of linear interpolators based on SRC."
  (loop :for i :from 0 :below (1- (length src))
        :collect (linear-interpolator (cons i (aref src i))
                                      (cons (1+ i) (aref src (1+ i))))))

(defun src->dst (src w_d)
  "Interpolate the 1D array SRC to a destination array of size W_D."
  (let* (
         ;; Where the interpolated points go.
         (dst (make-array w_d :initial-element nil))
         
         ;; Width of the source data.
         (w_s (length src))

         ;; The number of gaps to be filled.
         (gap-count (1- w_s))
         
         ;; The total number of spaces to be filled.
         (delta-w (- w_d w_s))
         
         ;; The width of each gap (this will get computed).
         (gap-widths nil)
         
         ;; Linear interpolation functions.
         (lerps (make-linear-interpolators src)))
    
    ;; Compute the gap width and the extra slack.
    (multiple-value-bind (base-gap-size extra-spaces)
        (floor delta-w gap-count)
      
      ;; Make an array of initial gap widths.
      (setf gap-widths (make-array gap-count :initial-element base-gap-size))
      
      ;; Distribute the slack spaces to the gap sizes until we run
      ;; out. Since we will have less slack than the total number of
      ;; gaps, we can just add 1 to the first few gaps.
      (dotimes (i extra-spaces)
        (incf (aref gap-widths i)))

      ;; Interpolate the gaps.
      (loop :with offset := 0
            :for n :from 0
            :for lerp :in lerps
            :for gap-width :across gap-widths
            :do (progn
                  ;; Fill in point from SRC.
                  (setf (aref dst offset)
                        (aref src n))
                  
                  ;; Interpolate the the gap.
                  (dotimes (i gap-width)
                    (setf (aref dst (+ 1 i offset))
                          (funcall lerp
                                   (+ n
                                      (/ (+ 2 i)
                                         (+ 2 gap-width))))))
                  
                  ;; Increment the offset.
                  (incf offset (1+ gap-width))))
      
      ;; Fill in the end point.
      (setf (aref dst (1- w_d)) (aref src (1- w_s)))

      ;; Return the destination array.
      dst)))
