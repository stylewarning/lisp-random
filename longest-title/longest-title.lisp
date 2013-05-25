;;;; longest-title.lisp
;;;; Copyright (c) 2013 Robert Smith

;;;; Requires :RECUR and :SPLIT-SEQUENCE.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Word Index ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *word->index* (make-hash-table :test 'equal))
(defparameter *index->word* (make-hash-table :test 'eql))

(let ((count 0))
  (defun index-word! (word)
    (unless (gethash word *word->index*)
      (setf (gethash word *word->index*) count
            (gethash count *index->word*) word)
      (incf count))))

(defun word->index (word)
  (gethash word *word->index*))

(defun index->word (index)
  (gethash index *index->word*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; Title Index ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct title contents start end added-length)

(defparameter *titles* (make-array 1024 :adjustable t
                                        :fill-pointer 0))
(defparameter *titles-starting-with* (make-hash-table :test 'eql))
(defparameter *titles-ending-with* (make-hash-table :test 'eql))

(defun index-title! (title)
  (let ((start (title-start title))
        (end   (title-end   title)))
    (vector-push-extend title *titles*)
    (push title (gethash start *titles-starting-with*))
    (push title (gethash end   *titles-ending-with*))))

(defun titles-starting-with (word-index)
  (gethash word-index *titles-starting-with*))

(defun titles-ending-with (word-index)
  (gethash word-index *titles-ending-with*))

(defun random-title ()
  (aref *titles* (random (length *titles*))))

(defun parse-titles (filename)
  (with-open-file (s filename :direction :input)
    (recur:recur rec ((next (read-line s nil nil)))
      (if (null next)
          'done
          (let ((title-words (split-sequence:split-sequence #\Space next)))
            ;; Index the title words
            (mapc #'index-word! title-words)
            
            ;; Construct the title and index it
            (let* ((encoded-title (mapcar #'word->index title-words))
                   (title (make-title :contents encoded-title
                                      :start (first encoded-title)
                                      :end (car (last encoded-title))
                                      :added-length (reduce #'+
                                                            (cdr title-words)
                                                            :key #'length
                                                            :initial-value 0))))
              (index-title! title))

            (rec (read-line s nil nil))))))
  
  ;; Normalize the indexes for effective greedy search.
  (loop :for k :being :the :hash-keys :in *titles-starting-with*
        :do (setf (gethash k *titles-starting-with*)
                  (sort (gethash k *titles-starting-with*)
                        #'>
                        :key #'title-added-length)))
  'done)

(defun title-length (title)
  (+ (length (index->word (title-start title)))
     (title-added-length title)))

(defparameter *title->maximal-chain* (make-hash-table :test 'eql))

(defun search-chain-cache (title history)
  (find-if (lambda (result)
             (not (intersection (cdr result) history)))
           (gethash title *title->maximal-chain*)))

(defun cache-chain! (title len-chain)
  (push len-chain (gethash title *title->maximal-chain*))
  (setf (gethash title *title->maximal-chain*)
        (sort (gethash title *title->maximal-chain*)
              #'>
              :key #'car))
  len-chain)

(defun random-elt (seq)
  (elt seq (random (length seq))))

(defun title->string (title)
  (reduce (lambda (x y)
            (concatenate 'string x " " y))
          (title-contents title)
          :key #'index->word))

(defun title->position (title)
  (position title *titles*))

(defun find-chains (title)
  (format t "Trying title: ~S (~S)~%" title (title->string title))
  (let ((longest 0)
        (longest-chain nil))
    (labels ((find-chain (title length history depth)
               (let* ((end   (title-end   title))
                      (candidates (set-difference (titles-starting-with end)
                                                  history)))
                 (if (null candidates)
                     (when (> length longest)
                       (setf longest length
                             longest-chain (reverse history))
                       (format t "~&Found ~D-chain at depth ~D: ~S~%"
                               longest
                               depth
                               (mapcar #'title->position longest-chain)))
                     (mapc (lambda (candidate)
                             (find-chain candidate
                                         (+ length (title-added-length candidate))
                                         (cons candidate history)
                                         (1+ depth)))
                           candidates)))))
      (find-chain title
                  (title-length title)
                  (list title)
                  0))))


;; #S(TITLE :CONTENTS (738 996 2939 74) :START 738 :END 74 :ADDED-LENGTH 8) ("KILL BILL VOL 2")
;; #S(TITLE :CONTENTS (4247 105 74) :START 4247 :END 74 :ADDED-LENGTH 7)
