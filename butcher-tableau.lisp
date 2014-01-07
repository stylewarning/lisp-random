;;;; butcher-tableau.lisp
;;;; Copyright (c) 2014 Robert Smith

(defparameter *rk4-tableau* '((0                  )
                              (1/2 1/2            )
                              (1/2 0   1/2        )
                              (1   0   0   1      )
                              (nil 1/6 1/3 1/3 1/6)))

(defstruct butcher-tableau
  s
  a
  b
  c)

(defun parse-tableau (tab)
  (let* ((s  (1- (length tab))))
    (flet ((populate-table (entries)
             (loop :with a := (make-array (list s (1- s)) :initial-element nil)
                   :for i :from 1 :to s
                   :for row :in entries
                   :do (loop :for j :from 1 :to (1- s)
                             :for node :in row
                             :do (setf (aref a (1- i) (1- j)) node))
                   :finally (return a))))
      (make-butcher-tableau :s s
                            :b (coerce (cdar (last tab)) 'vector)
                            :c (coerce (butlast (mapcar #'first tab)) 'vector)
                            :a (populate-table (mapcar #'cdr (butlast tab)))))))

(defun times (a b)
  (cond
    ((or (eql 0 a)
         (eql 0 b)) 0)
    ((eql 1 a) b)
    ((eql 1 b) a)
    (t `(* ,a ,b))))

(defun plus (a b)
  (cond
    ((eql 0 a) b)
    ((eql 0 b) a)
    (t `(+ ,a ,b))))

(defun k-name (n)
  (intern (format nil "K~D" n)))

(defun generate-k (l bt &key (tn 'tn)
                             (yn 'yn)
                             (h  'h)
                             (f  'f))
  (labels ((first-arg ()
             (plus tn (times (aref (butcher-tableau-c bt) (1- l)) h)))
           
           (second-arg ()
             (loop :for i :from 0 :to (1- l)
                   :for sum := yn :then (plus sum (times (aref (butcher-tableau-a bt) (1- l) (1- i))
                                                         (k-name i)))
                   :finally (return sum))))
    (times h `(funcall ,f ,(first-arg) ,(second-arg)))))

(defun generate-y (bt &key (tn 'tn)
                           (yn 'yn)
                           (h  'h)
                           (f  'f))
  (declare (ignore f h tn))
  (loop :for i :from 0 :to (butcher-tableau-s bt)
        :for y := yn :then (plus y (times (aref (butcher-tableau-b bt) (1- i))
                                          (k-name i)))
        :finally (return y)))

(defun generate-rk-method (bt y0 t0 h)
  `(let* (,@(loop :for l :from 1 :to (butcher-tableau-s bt)
                  :collect (list (k-name l) (generate-k l bt))))
     (values
      (+ ,t0 ,h)
      ,(generate-y bt :yn y0 :tn t0 :h h))))
