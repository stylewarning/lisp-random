(defmacro execution-time (&body body)
  "Return the number of milliseconds it takes to execute BODY. Also
returns the result as the second value."
  (let ((tm (gensym))
        (res (gensym)))
    ;; This is coded very particularly to be as accurate as possible.
    `(let* ((,tm (get-internal-real-time))
            (,res (progn ,@body))
            (,tm (floor (* 1000 (- (get-internal-real-time) ,tm))
                        internal-time-units-per-second)))
       (values ,tm ,res))))

(defun string-concatenate (&rest args)
  "Concatenate all strings in ARGS. Useful for when you need to use
APPLY."
  (reduce #'(lambda (x y) (concatenate 'string x y))
          args))

(defun print-table (data
                    &key
                    (stream *standard-output*)
                    (header nil)
                    (directive " ~8@A"))
  "A rickety table printer. Print DATA, a list of constant-sized
lists, and print them as vertical columns, possibly with HEADER
strings and a special DIRECTIVE for printing all values."
  (let* ((columns (length (car data)))
         (fmt (apply #'string-concatenate
                     (append '("~:{~&")
                             (make-list columns :initial-element directive)
                             '("~}")))))
    (format stream fmt (append (list header) data))))

