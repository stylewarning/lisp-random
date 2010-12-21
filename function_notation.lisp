;; Dispatch on the `{' character to LAMBDA-BRACE
(set-macro-character
 #\{
 (lambda (stream char)
   (declare (ignore char))
   (let* ((lst (read-delimited-list #\} stream t)))
     (lambda-brace lst))))

(defun lambda-brace (elements)
  ;; Collect all of the variables of the lambda
  (let ((args (loop :while (and (not (eq (car elements) '->))
                                elements)
                    :collect (prog1 (car elements)
                               (setf elements (cdr elements))))))
    ;; Construct the lambda
    (if (null elements)
        `(lambda () ,@args)
      `(lambda (,@args) ,@(cdr elements)))))

;; Let `]' be equivalent to `)'
(set-macro-character
 #\}
 (get-macro-character #\)))

;;; EXAMPLES
;; CL-USER> (mapcar {x -> (1+ x)} '(1 2 3 4))
;; (2 3 4 5)
;; CL-USER> (mapcar {x y -> (+ (expt x 2) (expt y 2))} '(1 2 3 4) '(5 6 7 8))
;; (26 40 58 80)
