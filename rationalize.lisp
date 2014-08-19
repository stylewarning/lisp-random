;;;; rationalize.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith

(defun naive-rationalize (float)
  "Rationalize the floating point number FLOAT naively."
  (rationalize float))

(defun rational-split (r)
  "Convert a possibly improper rational R into a proper rational (A P/Q) such that R = A + P/Q and 0 <= P/Q < 1."
  (multiple-value-list (truncate r)))

(defun rational->cf (r)
  "Convert a rational number R into its standard continued fraction representation, represented as a vector of integers."
  (check-type r rational)
  (loop
    :for proper := (rational-split r)
    :collect (first proper) :into cf
    :sum 1 :into length
    :until (zerop (second proper))
    :do (setf r (/ (second proper)))
    :finally (return (make-array length :element-type 'integer
                                        :initial-contents cf))))

(defun cf->rational (cf &key end)
  "Convert a continued fraction CF into a rational."
  (reduce (lambda (next sum)
            (+ next (/ sum)))
          cf
          :end end
          :from-end t))

(defun map-cf-convergents (f cf)
  "Map through the convergents of the continued fraction CF, calling the function F on each. The convergents will be iterated through in standard order (least precise to most precise)."
  (loop :for end :from 1 :to (length cf)
        :do (funcall f (cf->rational cf :end end))))

(defun map-rational-convergents (f r)
  "Map through the convergents of the rational R, calling the function F on each. The convergents will be iterated through in standard order (least precise to most precise)."
  (map-cf-convergents f (rational->cf r)))

(defun nice-rationalize (float &key (tolerance (/ (expt (float-radix float)
                                                        (float-precision float)))))
  "Find a rational R such that

    | R - FLOAT | <= TOLERANCE

and R has a minimal denominator."
  (check-type float float)
  (let ((exact (naive-rationalize float)))
    (labels ((select-convergent (c)
               (when (<= (abs (- c exact)) tolerance)
                 (return-from nice-rationalize c))))
      (declare (dynamic-extent #'select-convergent))
      (map-rational-convergents #'select-convergent exact))))
