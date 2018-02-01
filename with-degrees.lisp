;;;; with-degrees.lisp
;;;;
;;;; Copyright (c) 2018 Robert Smith

;;; Exercise: http://www.watrophy.com/posts/10-Using-Degrees.html

;;; In order to do this exercise in CL, you need to get around issues
;;; with package locks.

(defmacro with-degrees (&body body)
  (let ((old-sin (gensym "OLD-SIN"))
        (old-cos (gensym "OLD-COS"))
        (old-tan (gensym "OLD-TAN")))
    `(let ((,old-sin #'sin)
           (,old-cos #'cos)
           (,old-tan #'tan))
       ;; Lexically disable package locks.
       (locally (declare (sb-ext:disable-package-locks sin cos tan))
         (labels ((sin (x)
                    (funcall ,old-sin (/ (* pi x) 180)))
                  (cos (x)
                    (funcall ,old-cos (/ (* pi x) 180)))
                  (tan (x)
                    (funcall ,old-tan (/ (* pi x) 180))))
           ;; Re-enable them for default behavior.
           (locally (declare (sb-ext:enable-package-locks sin cos tan))
             ,@body))))))
