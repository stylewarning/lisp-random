;;;; triangle-print.lisp
;;;; Copyright (c) 2013 Robert Smith

(defun triangle-print (name &key right)
  (let ((indent 0))
    (flet ((print-list (list)
             (format t "~vT~{~C~^ ~}~%" (* (if right 2 1)
                                           (incf indent)) list)))
      (mapl #'print-list (coerce name 'list)))))

;; CL-USER> (triangle-print "hello")
;;  h e l l o
;;   e l l o
;;    l l o
;;     l o
;;      o

;; CL-USER> (triangle-print "hello" :right t)
;;   h e l l o
;;     e l l o
;;       l l o
;;         l o
;;           o
