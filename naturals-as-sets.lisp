;;;; naturals-as-sets.lisp
;;;; Copyright (c) 2013 Robert Smith

;;; We use LSET instead of SET because (1) the internal representation
;;; is a List and (2) SET already means something in Lisp.
(defstruct (lset (:print-function (lambda (obj stream depth)
                                    (declare (ignore depth))
                                    (format stream "{~{~A~^ ~}}"
                                            (lset-elements obj))))
                 (:predicate lsetp))
  (elements nil :type list))

(defun lset (&rest arguments)
  (make-lset :elements arguments))

(defun lset-union (s1 s2 &key (test #'eql))
  (make-lset :elements (union (lset-elements s1)
                              (lset-elements s2)
                              :test test)))

(defvar *numbers* (let ((zero (make-lset))
                        (tbl (make-hash-table)))
                    (setf (gethash 0 tbl) zero)
                    tbl))

(defun natural-lset (n)
  (assert (and (integerp n)
               (not (minusp n))))
  (let ((res (gethash n *numbers*)))
    (if res
        res
        (let ((n-1 (natural-lset (1- n))))
          (lset-union n-1 (lset n-1))))))
