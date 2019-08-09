;;;; plot-distribution.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:!)

(defun plot-distribution (d &key (max-width 60)
                                 (width-value (alexandria:extremum d #'>))
                                 (stream *standard-output*))
  ;; D should be a vector of real values that sum to 1.
  (loop :with sum := 0.0d0
        :for length :from 0
        :for p :across d
        :for count := (round (* p max-width) width-value)
        :do (format stream "~5,' D |" length)
            (loop :repeat count :do (write-char #\* stream))
            (terpri stream)
            (incf sum p)
        :while (< sum 0.99999))
  (values))
