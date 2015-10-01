;;;; date-changes.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

(defun human-to-digital (human-month)
  "Convert the numerical representation of a month expressed by humans into a digital numerical representation."
  (1- human-month))

(defun digital-to-human (digital-month)
  "Convert the digital representation of a month to a representation used by humans."
  (1+ digital-month))



;; Part A

(defun change-date-a (month year increment)
  "Given a MONTH and YEAR as specified by humans, increment it by INCREMENT months. Return two values: the new month and year."
  (multiple-value-bind (delta-years new-month)
      (floor (+ increment (human-to-digital month)) 12)
    (values (digital-to-human new-month)
            (+ year delta-years))))


;; Part B

(defparameter *month-days*
  #(31 28 31 30 31 30 31 31 30 31 30 31)
  "The number of days in each month on a non-leap year.")

(defun leap-year-p (year)
  "Does YEAR represent a leap year?"
  (and (zerop (mod year 4))
       (if (zerop (mod year 100))
           (zerop (mod year 400))
           t)))

(defun days-in-month (month &optional year)
  "How many days are in the (digital) month MONTH?

If YEAR is supplied, then leap years will be taken into account."
  (let ((days (aref *month-days* month)))
    (if (or (null year)
            (/= 1 month)                ; February
            (not (leap-year-p year)))
        days
        (1+ days))))

(defun change (day month year increment)
  "Given a digital day, month, and year, and a number of days to increment, compute the new date, month, and year, taking leap years into account."
  (labels ((forward (day month year increment)
             (if (<= 12 month)
                 (forward day (- month 12) (1+ year) increment)
                 (let ((month-days (days-in-month month year)))
                   (if (< increment month-days)
                       (values increment month year)
                       (forward 0 (1+ month) year (- increment month-days))))))
           (backward (day month year increment)
             (if (minusp month)
                 (backward day (+ month 12) (1- year) increment)
                 (let ((prev-month-days
                         (if (zerop month)
                             (days-in-month 11 (1- year))
                             (days-in-month (1- month) year))))
                   (if (<= increment prev-month-days)
                       (values (- prev-month-days increment)
                               (mod (1- month) 12)
                               (if (zerop month) (1- year) year))
                       (backward 0
                                 (1- month)
                                 year
                                 (- increment prev-month-days)))))))
    (cond
      ;; Nothing to do.
      ((zerop increment) (values day month year))
      ;; Normalize the day so it is the first of the month.
      ((plusp day) (change 0 month year (+ increment day)))
      ;; If we increment forward, do one algorithm.
      ((plusp increment) (forward day month year increment))
      ;; If we increment backward, do another algorithm.
      ((minusp increment) (backward day month year (- increment))))))

(defun change-date-b (day month year increment)
  "Given the day DAY and month MONTH as specified by humans, as well as the year YEAR, increment it by INCREMENT number of days. Returns three values: the new date, month, and year."
  (multiple-value-bind (new-day new-month new-year)
      (change (human-to-digital day)
              (human-to-digital month)
              year
              increment)
    (values (digital-to-human new-day)
            (digital-to-human new-month)
            new-year)))
