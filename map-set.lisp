;;;; map-set.lisp
;;;; Copyright (c) 2013 Robert Smith

;;;; A data structure with (amortized) constant addition, removal,
;;;; membership, and random selection.
;;;;
;;;; The API is very similar to that of a hash table.

(defstruct (map-set (:constructor make-map-set ())
                    (:copier nil)
                    (:print-function (lambda (obj stream depth)
                                       (declare (ignore depth))
                                       (print-unreadable-object
                                           (obj stream :type t)
                                         (format stream
                                                 "of ~D element~:P"
                                                 (map-set-size obj))))))
  (table (make-hash-table :test 'equalp)               :type hash-table)
  (index (make-array 16 :adjustable t :fill-pointer 0) :type vector)
  (size  0                                             :type unsigned-byte))

(defun ms-count (ms)
  "Return the cardinality/size of the map-set MS."
  (map-set-size ms))

(defun ms-member-p (ms item)
  "Is ITEM a member of the map-set MS?"
  (and (gethash item (map-set-table ms))
       t))

(defun ms-add! (ms item)
  "Add the item ITEM to the map-set MS."
  (unless (ms-member-p ms item)
    (setf (gethash item (map-set-table ms))
          (map-set-size ms))
    (incf (map-set-size ms))
    (vector-push-extend item (map-set-index ms)))
  ms)

(defun ms-remove! (ms item)
  "Remove the item ITEM from the map-set MS."
  (when (ms-member-p ms item)
    (let* ((idx (map-set-index ms))
           (tbl (map-set-table ms))
           (location (gethash item tbl)))
      ;; Move the last element in the index to the position of the
      ;; to-be deleted item.
      (setf (aref idx location)
            (aref idx (1- (map-set-size ms))))
      
      ;; Pop the last item off the index.
      (vector-pop (map-set-index ms))
      
      ;; Decrease the element count.
      (decf (map-set-size ms))
      
      ;; Remove the item from the record.
      (remhash item tbl)
      
      ;; Update the position of the moved element.
      (setf (gethash (aref idx location) tbl) location)))
  ms)

(defun ms-random (ms)
  "Select a random element of the map-set MS."
  (let ((size (map-set-size ms)))
    (if (zerop size)
        nil
        (aref (map-set-index ms)
              (random size)))))

(defun ms-map (type f ms)
  "Map the unary function F across the map-set MS with a result type
TYPE."
  (map type f (map-set-index ms)))

(defun ms-for-each (f ms)
  "Apply the function F to each item in the map-set MS."
  (map nil f (map-set-index ms)))
