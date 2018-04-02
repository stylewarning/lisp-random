;;;; look-and-say.lisp
;;;;
;;;; Copyright (c) 2018 Robert Smith

;;; We are going to replace LOOK AND SAY with LAS.

(declaim (optimize speed (safety 0) (debug 0) (space 0)))

(deftype las-number ()
  '(mod 4))

(deftype las-word ()
  `(simple-array las-number (*)))

(defun make-las-word (length)
  (make-array length :element-type 'las-number
                     :initial-element 0))

(defun next-las-length (current)
  (declare (type las-word current))
  (loop :with num-things :of-type fixnum := 0
        :with last-seen :of-type las-number := 0
        :for x :across current
        :when (/= last-seen x)
          :do (setf last-seen x)
              (incf num-things)
        :finally (return (* 2 num-things))))

;; CHALLENGE #1
(defun next-las-word (las-word)
  (declare (type las-word las-word))
  (assert (plusp (length las-word)))
  (let* ((current-length (length las-word))
         (next-length (next-las-length las-word))
         (next-las-word (make-las-word next-length)))
    (labels ((%rle (index item count dest-ptr)
               (declare (type fixnum count))
               (if (= index current-length)
                   (progn
                     (setf (aref next-las-word dest-ptr) count)
                     (setf (aref next-las-word (1+ dest-ptr)) item)
                     next-las-word)
                   (let ((next-item (aref las-word index))
                         (next-index (1+ index)))
                     (if (= item next-item)
                         (progn
                           (%rle next-index next-item (1+ count) dest-ptr))
                         (progn
                           (setf (aref next-las-word dest-ptr) count)
                           (setf (aref next-las-word (1+ dest-ptr)) item)
                           (%rle next-index next-item 1 (incf dest-ptr 2))))))))
      (%rle 1 (aref las-word 0) 1 0))))

(defun print-table ()
  (loop 
    :with time := (get-internal-real-time)
    :for i :from 0
    :for x :of-type las-word := (make-array 1 :element-type 'las-number
                                              :initial-element 1)
      :then (next-las-word x)
    :do (format t "~D: ~D [~D ms]~%"
                i 
                (length x)
                (/ (* 1000 (- (get-internal-real-time) time))
                   internal-time-units-per-second))
        (setf time (get-internal-real-time))))
