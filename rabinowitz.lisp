;;;; rabinowitz.lisp
;;;; Copyright (c) 2013 Robert Smith

(defun rabinowitz (n &key save-digits)
  (let* ((len (1+ (floor (* 10 n) 3)))
         (a (make-array (1+ len) :initial-element 2
                                 :element-type '(unsigned-byte 32)
                        ))
         (predigit 0)
         (queue nil)
         (queue-length 0)
         (digits (make-array (if save-digits (1- n) 0)
                             :initial-element 0
                             :element-type '(integer 0 9)))
         (cursor 0))
    (declare (type (unsigned-byte 32) n len cursor queue-length))
    
    (setf (aref a 0) 0)                 ; Unused.
    
    (flet ((queue-up (n)
             (push n queue)
             (incf queue-length))
           
           (release ()
             (when save-digits
               ;; We have recorded the length of QUEUE as it was
               ;; built. Therefore, we can avoid reversing it by
               ;; giving the index offset right away.
               (let ((new-cursor (+ cursor queue-length)))
                 (dolist (d queue)
                   (setf (aref digits (1- (+ cursor queue-length)))
                         d)
                   (decf queue-length))

                 ;; Reset everything.
                 (setf queue        nil
                       queue-length 0
                       cursor       new-cursor)))))
      (declare (inline queue-up release))
      
      (loop
        :repeat n
        :do (progn
              (loop :with carry := 0
                    :for i :from len :downto 2
                    :do (multiple-value-bind (quo rem)
                            (floor (+ carry (* 10 (aref a i)))
                                   (1- (* 2 i)))
                          (setf (aref a i) rem)
                          (setf carry (* quo (1- i))))
                    :finally  (setf (aref a 1) (+ carry (* 10 (aref a 1)))))

              (multiple-value-bind (quo rem)
                  (floor (aref a 1) 10)
                (setf predigit   quo
                      (aref a 1) rem)

                (cond
                  ((and (/= quo 9) (/= quo 10))
                   (release)
                   (queue-up quo))
                  ((= quo 9)
                   (queue-up quo))
                  ((= quo 10)
                   (setf predigit 0)
                   (map-into queue (lambda (x)
                                     (declare (type (unsigned-byte 4) x))
                                     (mod (1+ x) 10)) queue)
                   (queue-up 0)
                   (release)))))))
    (values predigit digits)))

;;; 26.923: Base
;;; 26.719: 1-index
;;; 19.024: OPTIMIZE
;;; 19.294: #1
;;;

;;; SBCL
;;; 7.5
;;; 4.4 ; remove MAP-INTO
