;;;; long-multiplication.lisp
;;;;
;;;; Copyright (c) 2018 Robert Smith

;;; Challenge http://www.watrophy.com/posts/29-Multiplying-Long-Hand.html

(defun normalize (digits)
  (let ((p (position-if #'plusp digits :from-end t)))
    (cond
      (p (subseq digits 0 (1+ p)))
      ((every #'zerop digits) '(0))
      (t digits))))

(defun digits (n)
  (labels ((%digits (n list)
             (if (zerop n)
                 (nreverse list)
                 (multiple-value-bind (quo rem)
                     (floor n 10)
                   (%digits quo (cons rem list))))))
    (if (zerop n)
        '(0)
        (%digits n nil))))

(defun undigits (dlist)
  (labels ((%undigits (dlist b r)
             (if (null dlist)
                 r
                 (%undigits (rest dlist)
                            (* 10 b)
                            (+ r (* b (first dlist)))))))
    (%undigits dlist 1 0)))

(defun propagate-carries (digits)
  (labels ((rec (digits carry)
             (if (null digits)
                 (if (zerop carry)
                     nil
                     (digits carry))
                 (multiple-value-bind (new-carry normalized-d)
                     (floor (+ carry (first digits)) 10)
                   (cons normalized-d (rec (rest digits) new-carry))))))
    (rec digits 0)))

(defun *by (n)
  (lambda (x)
    (* n x)))

(defun long-add (terms)
  (labels ((next-digit (terms)
             (loop :for term :in terms
                   :sum (or (first term) 0)))
           (rec (terms)
             (if (every #'null terms)
                 nil
                 (cons (next-digit terms)
                       (rec (mapcar #'rest terms))))))
    (propagate-carries (rec terms))))

(defun long-multiply-work (a b)
  (let ((terms
          (loop :for i :from 0
                :for bi :in b
                :collect (normalize
                          (append (make-list i :initial-element 0)
                                  (propagate-carries (mapcar (*by bi) a)))))))
    (values (long-add terms) terms)))

;;; Printing

(defun print-number (a)
  (loop :for d :in (reverse a)
        :do (format t "~D" d)))

(defun write-repeat (char n)
  (loop :repeat n :do (write-char char)))

(defun print-long-multiply (a b terms result)
  (let* ((a-len (length a))
         (b-len (length b))
         (zero (or (every #'zerop a)
                   (every #'zerop b)))
         (a-b (- a-len b-len))
         (first-vin-size (+ 2 (max a-len b-len)))
         (max-term-len (loop :for term :in terms :maximize (length term)))
         (result-len (length result))
         (multiplicand-offset (max 0 (- max-term-len (max a-len b-len) 2)))
         (term-offset (max 0 (- first-vin-size result-len)))
         (offset (- result-len max-term-len)))
    ;; first number
    (write-repeat #\Space (+ offset multiplicand-offset))
    (write-string "  ")
    (when (minusp a-b) (write-repeat #\Space (abs a-b)))
    (print-number a)
    (terpri)
    ;; second number
    (write-repeat #\Space (+ offset multiplicand-offset))
    (format t "x ")
    (when (plusp a-b) (write-repeat #\Space a-b))
    (print-number b)
    (terpri)
    ;; vinculum
    (write-repeat #\Space (+ offset multiplicand-offset))
    (write-repeat #\- first-vin-size)
    (terpri)
    ;; early exit
    (when zero
      (write-repeat #\Space (1- first-vin-size))
      (write-char #\0)
      (return-from print-long-multiply))
    ;; terms
    (dolist (term terms)
      (unless (every #'zerop term)
        (write-repeat #\Space (+ offset
                                 term-offset
                                 (- max-term-len (length term))))
        (print-number term)
        (terpri)))
    ;; vincukum
    (write-repeat #\- (+ term-offset result-len))
    (terpri)
    ;; result
    (write-repeat #\Space term-offset)
    (print-number result)
    (terpri)))

(defun long-multiply (a b)
  (multiple-value-bind (result terms)
      (long-multiply-work a b)
    (print-long-multiply a b terms result)
    result))

