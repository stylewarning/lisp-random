;;;; one-offs.lisp
;;;; Copyright (c) 2012 Robert Smith


;;; Incremental Fisher-Yates
(defun make-fisher-yates-generator (max-value)
  "Create a generator which will generate the values between 0 and
MAX-VALUE - 1 exactly once in shuffled order."
  (let ((choices (loop :for i :below max-value :collect i)))
    (lambda ()
      (unless (null choices)
        (let ((random-index (random (length choices))))
          (prog1 (nth random-index choices)
            ;; Remove the Nth element from our choices.
            (setf choices (delete-if (constantly t) choices
                                     :start random-index
                                     :count 1))))))))

