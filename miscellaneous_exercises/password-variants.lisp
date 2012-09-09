;;;; password-variants.lisp
;;;; Copyright (c) 2012 Robert Smith

;;;; Given a password string S and a mapping between letters and
;;;; letter variants, produce all variants of the string S.
;;;;
;;;; For example, given "fat", and the dictionary
;;;; 
;;;;   a -> @, 4
;;;;   e -> 3
;;;;
;;;; producea list containing the strings
;;;;
;;;;   fate, f@te, f4te
;;;;   fat3, f@t3, f4t3.
;;;;
;;;; If a string contains duplicate letters, then each combination for
;;;; each letter should appear. For example, "leet" with the above
;;;; substitution table should produce
;;;;
;;;;   leet l3et le3t l33t

(defun generate-full-substitution-table (string subs)
  (loop :for c :across string
        :for i :from 0
        :for m := (find c subs :key #'car)
        :when m
          :collect (cons m i) :into subs-idxs
        :finally (return (values (mapcar #'car subs-idxs)
                                 (mapcar #'cdr subs-idxs)))))

(defun generate-signature (string subs)
  (mapcar #'length subs))

(defun incrementer (signature)
  (labels ((propagate (radix sig carry accum)
             (cond
               ((or (null radix)
                    (null sig))
                (if (plusp carry)
                    (error "Radix overflow.")
                    (nreverse accum)))
               
               ;; No overflow...
               ((< (+ carry (car radix)) (car sig))
                (propagate (cdr radix)
                           (cdr sig)
                           0
                           (cons (+ carry (car radix))
                                 accum)))
               
               (t
                (propagate (cdr radix)
                           (cdr sig)
                           1
                           (cons 0 accum))))))
    (lambda (radix-rep)
      (propagate radix-rep signature 1 nil))))

(defun radix-rep->characters (radix-rep subs)
  (mapcar #'nth radix-rep subs))

(defun replace-char-at (str char pos)
  (replace str (string char) :start1 pos))

(defun replace-chars (str chars posns)
  (loop :for c :in chars
        :for p :in posns
        :for s := (replace-char-at str c p)
          :then (replace-char-at s c p)
        :finally (return str)))

(defun generate-passwords (string base-subs)
  (multiple-value-bind (full-subs idxs) 
      (generate-full-substitution-table string base-subs)
    (let* ((sig       (generate-signature full-subs))
           (potential (reduce #'* sig)) ; Number of radixes
           (inc       (incrementer sig)))
      (loop :repeat potential
            
            ;; Increment from 0 to the radix rep of POTENTIAL.
            :for radix := (mapcar (constantly 0) sig) ; start with (0 0 0 ...)
              :then (funcall inc radix)
            
            ;; We must copy the sequence since we are destructively
            ;; updating using REPLACE.
            :collect (copy-seq (replace-chars string
                                              (radix-rep->characters radix
                                                                     full-subs)
                                              idxs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *substitutions*
  '((#\a  #\@ #\4)
    (#\e  #\3)))

(defun test ()
  (assert (null (set-difference (generate-passwords "fate" *substitutions*)
                                '("fate" "f4te" "f@te"
                                  "fat3" "f4t3" "f@t3")
                                :test #'string=))))