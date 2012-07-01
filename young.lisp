;;; Young Tableaux
;;; young.lisp
;;; Copyright (c) 2012 Robert Smith

;;;;;;;;;;;;;;;;;;; YOUNG TABLEAUX data structure ;;;;;;;;;;;;;;;;;;;;

;;; Tableaux Rows

(deftype tableau-element ()
  'fixnum)

(deftype tableau-row ()
  '(vector tableau-element))

(defconstant +head-room+ 3)

(defun make-row (&rest elements)
  (let* ((len (length elements))
         (a (make-array (+ len +head-room+) :element-type 'fixnum
                                            :adjustable t
                                            :fill-pointer 0)))
    
    (dolist (x elements (the tableau-row a))
      (declare (type fixnum x))
      (vector-push x a))))

(declaim (ftype (function (tableau-row tableau-element) tableau-row)
                extend-row))
(defun extend-row (row element)
  (vector-push-extend element row)
  row)

;;; Tableaux

(defstruct (tableau (:conc-name tableau.)
                    (:print-function print-tableau)
                    (:constructor %make-tableau)
                    (:copier nil))
  (rows #() :type (vector tableau-row)))

(defun tableau-size (tab)
  (loop :for row :across (tableau.rows tab)
        :sum (length row)))

(defun print-tableau (obj stream depth)
  (declare (ignore depth))
  (print-unreadable-object (obj stream :type t)
    (if (zerop (tableau-size obj))
        (format stream "Empty")
        (progn
          (terpri stream)
          (loop :with size := (1+ (log (tableau-size obj) 10))
                :for row :across (tableau.rows obj)
                :do (loop :for e :across row
                          :do (progn
                                (format stream "~v,' D" size e))
                          :finally (terpri stream)))))))

(defun make-tableau ()
  (%make-tableau :rows
                 (make-array  +head-room+
                             :element-type 'tableau-row
                             :adjustable t
                             :fill-pointer 0)))

(defun add-tableau-row (tableau row)
  (vector-push-extend row (tableau.rows tableau))
  tableau)

(defun lists-to-tableau (lists)
  (let ((tab (make-tableau)))
    (mapc (lambda (list-row)
            (add-tableau-row tab (apply #'make-row list-row)))
          lists)
    tab))

(defun tableau-cell (tableau row col)
  (aref (aref (tableau.rows tableau) (1- row))
        (1- col)))


(defun tableau-add-row (tableau new-cell)
  (add-tableau-row tableau (make-row new-cell))
  tableau)

(defun tableau-delete-row (tableau)
  (vector-pop (tableau.rows tableau))
  tableau)

(defun tableau-add-column (tableau row-number new-cell)
  (extend-row (aref (tableau.rows tableau) (1- row-number))
              new-cell)
  tableau)

(defun tableau-delete-column (tableau row-number)
  (vector-pop (aref (tableau.rows tableau) (1- row-number)))
  tableau)
