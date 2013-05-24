;;;; map-set.lisp
;;;; Copyright (c) 2013 Robert Smith

;;;; A data structure with (amortized) constant addition, removal,
;;;; membership, and random selection.

(defstruct (map-set (:constructor make-map-set ())
                    (:copier nil))
  (table (make-hash-table :test 'equal)                :type hash-table)
  (index (make-array 16 :adjustable t :fill-pointer 0) :type vector)
  (size  0                                             :type unsigned-byte))

(defun ms-cardinality (ms)
  "Return the cardinality/size of the map-set MS."
  (map-set-size ms))

(defun ms-member (ms item)
  "Is ITEM a member of the map-set MS?"
  (and (gethash item (map-set-table ms))
       t))

(defun ms-add! (ms item)
  "Add the item ITEM to the map-set MS."
  (unless (ms-member ms item)
    (setf (gethash item (map-set-table ms))
          (map-set-size ms))
    (incf (map-set-size ms))
    (vector-push-extend item (map-set-index ms)))
  ms)

(defun ms-remove! (ms item)
  "Remove the item ITEM from the map-set MS."
  (when (ms-member ms item)
    (let* ((idx (map-set-index ms))
           (tbl (map-set-table ms))
           (location (gethash item tbl)))
      (setf (aref idx location)
            (aref idx (1- (map-set-size ms))))
      (vector-pop (map-set-index ms))
      (decf (map-set-size ms))
      (remhash item tbl)
      (setf (gethash (aref idx location) tbl) location)))
  ms)

(defun ms-random (ms)
  "Select a random element of the map-set MS."
  (let ((size (map-set-size ms)))
    (if (zerop size)
        nil
        (aref (map-set-index ms)
              (random size)))))
