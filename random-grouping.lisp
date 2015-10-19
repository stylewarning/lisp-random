;;;; random-grouping.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

(defun random-grouping (num-bins list)
  (let ((length (length list)))
    (assert (<= num-bins length))
    (let ((bins      (make-array num-bins :initial-element nil))
          (bin-sizes (make-array num-bins :initial-element 0)))
      (labels ((place-in-bin (item bin-number)
                 (push item (aref bins bin-number))
                 (incf (aref bin-sizes bin-number)))

               (place-in-random-bin (item)
                 (place-in-bin item (random num-bins)))

               (retrieve-random-from-bin (bin-number)
                 (let ((bin-size (aref bin-sizes bin-number)))
                   (assert (plusp bin-size))
                   (let ((random-item-index (random bin-size))
                         (items             (aref bins bin-number)))
                     (decf (aref bin-sizes bin-number))
                     (if (zerop random-item-index)
                         (prog1 (car items)
                           (setf (aref bins bin-number) (cdr items)))
                         (let ((cons (nthcdr (1- random-item-index) items)))
                           (prog1 (cadr cons)
                             (rplacd cons (cddr cons))))))))

               (find-plentiful-bin ()
                 (let ((random-bin (random num-bins)))
                   (if (< 1 (aref bin-sizes random-bin))
                       random-bin
                       (find-plentiful-bin)))))
        ;; Fill the bins.
        (mapc #'place-in-random-bin list)
        
        ;; Fill any empty bins.
        (loop :for bin-number :from 0
              :for bin-size :across bin-sizes
              :when (zerop bin-size)
                :do (let ((stolen-item (retrieve-random-from-bin (find-plentiful-bin))))
                      (place-in-bin stolen-item bin-number)))
        
        ;; Return the filled bins.
        bins))))
