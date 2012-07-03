;;; Young Tableaux
;;; young.lisp
;;; Copyright (c) 2012 Robert Smith

(in-package #:common-lisp-user)

(defpackage #:young-tableaux
  (:use #:common-lisp)
  (:nicknames #:young)
  (:export #:tableau-element
           #:tableau-row
           #:tableau
           #:tableau-size
           #:tableau-height
           #:tableau-shape
           #:make-tableau
           #:lists-to-tableau
           #:tableau-cell
           #:tableau-add-row
           #:tableau-delete-row
           #:tableau-add-column
           #:tableau-delete-column
           #:perm-to-tableau))

(in-package #:young-tableaux)

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

(defun tableau-height (tab)
  (length (tableau.rows tab)))

(defun tableau-row-size (tab row)
  (length (aref (tableau.rows tab) (1- row))))

(defun tableau-shape (tab)
  (map 'list #'length (tableau.rows tab)))

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

(defun tableau-set-cell (tableau row col new-cell)
  (setf (aref (aref (tableau.rows tableau) (1- row))
              (1- col))
        new-cell))

(defsetf tableau-cell tableau-set-cell)

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

;;;;;;;;;;;;;;;;;;;;;; ROBINSON-SCHENSTED-KNUTH ;;;;;;;;;;;;;;;;;;;;;;

;;; Possible Optimizations
;;; 
;;; * Keep track of the height to avoid recomputing it each time.

(defun perm-to-tableau (perm)
  (labels ((bump! (tab i &optional (initial-row 1))
             (declare (type tableau tab)
                      (type tableau-element i)
                      (type fixnum initial-row))
             (if (> initial-row (tableau-height tab))
                 (tableau-add-row tab i)
                 (if (<= (tableau-cell tab ; Last cell...
                                       initial-row
                                       (tableau-row-size tab initial-row))
                         i)
                     (tableau-add-column tab initial-row i)
                     (let* ((row (aref (tableau.rows tab) (1- initial-row)))
                            (j (position-if (lambda (x) (> x i))
                                            row))
                            (row-j (aref row j)))
                       (declare (type fixnum j)
                                (type tableau-element row-j))
                       (setf (tableau-cell tab initial-row (1+ j)) i)
                       (bump! tab row-j (1+ initial-row)))))))

    ;; Bump each point in the word onto the tableau.
    (loop :with tab := (make-tableau)
          :for point :across (subseq (perm::perm.spec perm) 1)
          :do (bump! tab point)
          :finally (return tab))))
