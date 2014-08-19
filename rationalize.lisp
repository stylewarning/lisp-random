;;;; rationalize.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith
;;;;
;;;; This file contains an implementation of
;;;; NICE-RATIONALIZE. NICE-RATIONALIZE is a function which
;;;; rationalizes a float, but attempts to find a "nice" rational
;;;; number which has a small denominator. To do this, of course, the user 
;;;;
;;;; For example, consider the truncation of the rational 1/3: 0.33333.
;;;;
;;;; CL-USER> (rationalize 0.33333)
;;;; 33387/100162
;;;; CL-USER> (nice-rationalize 0.33333 :tolerance 1/10000)
;;;; 1/3


(defun exact-rationalize (float)
  "Rationalize the floating point number FLOAT exactly."
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

(defun nice-rationalize (float &key tolerance)
  "Find a rational R such that

    | R - FLOAT | <= TOLERANCE

and R has a minimal denominator.

If TOLERANCE is NIL, then standard rationalization will occur."
  (check-type float float)
  (check-type tolerance (rational (0)))
  (let ((exact (exact-rationalize float)))
    (labels ((select-convergent (c)
               ;; This comparison is okay because we are dealing with
               ;; exact quantities, as opposed to floats.
               (when (<= (abs (- c exact)) tolerance)
                 (return-from nice-rationalize c))))
      (declare (dynamic-extent #'select-convergent))
      (if (null tolerance)
          exact
          (map-rational-convergents #'select-convergent exact)))))
