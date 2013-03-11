;;;; rabinowitz.lisp
;;;; Copyright (c) 2013 Robert Smith

(defun rabinowitz (n &key save-digits)
  (let* ((len (1+ (floor (* 10 n) 3)))
         (a (make-array len :initial-element 2))
         (predigit 0)
         (queue nil)
         (digits (if save-digits
                     (make-array (1- n) :initial-element 0
                                   :element-type '(integer 0 9))
                     #()))
         (queue-length 0)
         (cursor 0))
    (flet ((queue-up (n)
             (push n queue)
             (incf queue-length))
           (release ()
             (when save-digits
               (let ((new-cursor (+ cursor queue-length)))
                 (dolist (d queue)
                   (setf (aref digits (1- (+ cursor queue-length)))
                         d)
                   (decf queue-length))
                 (setf queue        nil
                       queue-length 0
                       cursor       new-cursor)))))
      (loop
        :repeat n
        :do (let ((q 0))
              (map-into a (lambda (x) (* 10 x)) a)
              (loop :for i :from len :downto 2
                    :do (multiple-value-bind (quo rem)
                            (floor (aref a (1- i))
                                   (1- (* 2 i)))
                          (setf (aref a (1- i)) rem)
                          (setf q quo)
                          (incf (aref a (- i 2))
                                (* q (1- i)))))

              (multiple-value-bind (quo rem)
                  (floor (aref a 0) 10)
                (setf q quo)
                (setf (aref a 0) rem)
                (setf predigit q))

              (cond
                ((and (/= q 9) (/= q 10))
                 (release)
                 (queue-up q))
                ((= q 9)
                 (queue-up q))
                ((= q 10)
                 (setf predigit 0)
                 (map-into queue (lambda (x) (mod (1+ x) 10)) queue)
                 (queue-up 0)
                 (release))))))
    (values predigit digits)))
