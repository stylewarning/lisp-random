;;;; cxr.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

;;; Define arbitrarily-ordered CAR/CDR combinations.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun cxr-form (n i &optional (var 'x))
    "Produce Ith the CxR form of N bits. As a second value, give the ideal name of a function whose value is that form."
    (check-type n (integer 1))
    (check-type i unsigned-byte)
    (check-type var symbol)
    (loop :with form := var
          :with name := (make-string (+ 2 n))
          :for k :from n :downto 1
          :for bit := (logand 1 i)
          :do (setf 
               form (if (zerop bit)
                        `(car ,form)
                        `(cdr ,form))
               (aref name k) (if (zerop bit) #\A #\D)
               i (ash i -1))
          :finally
             (setf (aref name 0) #\C
                   (aref name (1+ n)) #\R)
             (return (values form name)))))

(defmacro define-cxr (n)
  "Define all CxR function definition forms of order N."
  (check-type n (integer 1))
  (let ((x (gensym "X")))
    `(progn
       ,@(loop :for i :below (expt 2 n)
               :collect (multiple-value-bind (form name)
                            (cxr-form n i x)
                          `(defun ,(intern name *package*) (,x)
                             ,form))))))
