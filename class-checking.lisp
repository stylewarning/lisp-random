(defun gf-specializers (gf &key names?)
  (mapcar (lambda (m)
            (mapcar (if names?
                        #'class-name
                        #'identity) (sb-mop:method-specializers m)))
          (sb-mop:generic-function-methods gf)))

(defun gf-specializers-nth (n gf &key names?)
  (mapcar (lambda (l) (nth n l)) (gf-specializers gf :names? names?)))

(defun class-subclasses-aux (class)
  (qtl:flatten
   (let ((subclasses (sb-mop:class-direct-subclasses class)))
     (when (car subclasses)
       (cons subclasses
             (let ((subsubclasses (mapcar #'class-subclasses subclasses)))
               (when (car subsubclasses)
                 subsubclasses)))))))

(defun class-subclasses (class &key names?)
  "All descendents/subclasses of CLASS."
  (if names?
      (mapcar #'class-name (class-subclasses-aux class))
      (class-subclasses-aux class)))

;;; This function is not quite right because given class C with
;;; subclass K, if GF specializes for C, but doesn't directly
;;; specialize for K, it's not an issue. This function, however, will
;;; return NIL in such a case since the subclass hierarchy is
;;; basically ignored.
(defun gf-specializes-all-subclasses-of (class gf &key (argument 0))
  "Does the generic function GF specialize all subclasses of CLASS?"
  (subsetp (class-subclasses class :names? t)
           (gf-specializers-nth argument gf :names? t) :test #'equal))
