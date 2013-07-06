;;(ql:quickload :quickutil)

(eval-when (:compile-toplevel)
  (qtlc:utilize :utilities '(:equivalence-classes
                             :transpose)))

(defvar *words-file* "/usr/share/dict/words")

(defvar *word-table* (make-hash-table))

(defun load-words ()
  (with-open-file (s *words-file* :direction :input)
    (loop :for word := (read-line s nil nil nil) :then (read-line s nil nil nil)
          :for total :from 0
          :while word
          :do (let ((normalized (string-upcase word)))
                (push normalized
                      (gethash (length normalized) *word-table*)))
          :finally (return total))))

(defun reverse-equal (a b)
  (string= a (reverse b)))

(defun palindromep (word)
  (reverse-equal word word))

(defun normalize-table ()
  (maphash (lambda (k v)
             (format t ";;; Normalizing entry ~D of length ~D~%" k (length v))
             (setf (gethash k *word-table*)
                   (remove-if #'null
                              (mapcar (lambda (class)
                                        (let ((len (length class)))
                                          (if (= 1 len)
                                              (if (palindromep (first class))
                                                  (list (first class)
                                                        (first class))
                                                  nil)
                                              class)))
                                      (qtl:equivalence-classes
                                       #'reverse-equal
                                       (delete-duplicates v :test #'string=))))))
           *word-table*))


(defun letters-needed (word)
  (coerce (subseq word 1 (ceiling (length word) 2)) 'list))

(defun incrementer (signature)
  (labels ((propagate (radix sig carry accum)
             (cond
               ((or (null radix)
                    (null sig))
                (if (plusp carry)
                    nil
                    (nreverse accum)))
               
               ;; No overflow...
               ((< (+ carry (car radix)) (car sig))
                (propagate (cdr radix)
                           (cdr sig)
                           0
                           (cons (+ carry (car radix))
                                 accum)))
               
               ;; Overflow... Propagate the carries forward.
               (t
                (propagate (cdr radix)
                           (cdr sig)
                           1
                           (cons 0 accum))))))
    (lambda (radix-rep)
      (propagate radix-rep signature 1 nil))))

(defun map-products (f seqs)
  (let ((increment (incrementer (mapcar #'length seqs)))
        (initial (make-list (length seqs) :initial-element 0)))
    (loop :for idx := initial :then (funcall increment idx)
          :while idx
          :do (funcall f (mapcar #'elt seqs idx)))))

(defun find-applicable-words-of-length (length letter)
  (loop :for (word rev) :in (gethash length *word-table*)
        :when (char= letter (char word 0))
          :collect word
        :when (and (not (string= word rev))
                   (char= letter (char rev 0)))
          :collect rev))

(defun transpose-solution (solution)
  (mapcar (lambda (chars)
            (coerce chars 'string))
          (qtl:transpose (mapcar (lambda (word)
                                   (coerce word 'list))
                                 solution))))

(defun 2d-palindromic-p (solution)
  (every #'string= solution (transpose-solution solution)))

(defun find-2d-palindromes-for-word (word)
  (labels ((compute-applicable-words (word)
             (let ((words
                     (mapcar (lambda (letter)
                               (find-applicable-words-of-length (length word)
                                                                letter))
                             (letters-needed word))))
               (if (notany #'null words)
                 words
                 nil)))
           (full-solution-from-partial (solution)
             (append (cons word solution)
                     (if (evenp (length word))
                         (reverse (mapcar #'reverse (cons word solution)))
                         (cdr (reverse (mapcar #'reverse (cons word solution)))))))
           (print-solution (partial-solution)
             (let ((full (full-solution-from-partial partial-solution)))
               (when (2d-palindromic-p full)
                 (format t "~{~A~%~}~%" full)))))
    (let ((words (compute-applicable-words word)))
      (when words
        (map-products #'print-solution words)))))

(defun find-2d-palindromes (length)
  (loop :for (word rev) :in (gethash length *word-table*)
        :do (find-2d-palindromes-for-word word)
        :unless (string= word rev)
          :do (find-2d-palindromes-for-word rev)))

(defun write-solutions-to-file (filename)
  (with-open-file (*standard-output* filename :direction :output
                                              :if-exists :supersede
                                              :if-does-not-exist :create)
    (loop :for length :from 4 :to (hash-table-count *word-table*)
          :do (progn
                (format t "SOLUTIONS OF LENGTH ~D~%~%" length)
                (find-2d-palindromes length)
                (terpri)
                (terpri)))))
