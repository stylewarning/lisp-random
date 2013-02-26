;;;; dfa.lisp
;;;; Copyright (c) 2013 Robert Smith


;;;; Special edges

(defconstant nul (code-char 0)
  "No transition.")

(defconstant eps (code-char 1)
  "Epsilon transition.")

(defun nul? (char)
  (char= char nul))

(defun eps? (char)
  (char= char eps))


;;;; Adjacency matrix manipulation

(defun adjacency-matrix (vertex-count &optional contents)
  (make-array (list vertex-count vertex-count)
              :initial-element nul
              :initial-contents contents))

(defun transition (matrix from to)
  (aref matrix from to))

(defsetf transition (matrix from to) (new-val)
  (setf (aref matrix from to) new-val))

(defun matrix-size (matrix)
  (array-dimension matrix 0))

;;;; Table 1

(defun inputs (matrix)
  (let ((size (matrix-size matrix))
        (inputs nil))
    (dotimes (to size inputs)
      (dotimes (from size)
        (let ((char (transition matrix from to)))
          (unless (or (nul? char)
                      (eps? char))
            (pushnew char inputs :test #'char=)))))))

(defstruct (nfa-state (:constructor %nfa-state))
  inputs
  state-vector)

(defun make-nfa-state (matrix)
  (let* ((inputs (inputs matrix))
         (state-vec (make-array (matrix-size matrix))))
    (%nfa-state :inputs inputs
                :state-vector 
                (map-into state-vec
                          (lambda (x)
                            (declare (ignore x))
                            (make-array (1+ (length inputs))))
                          state-vec))))

(defun refine-nfa-state (matrix state)
  (labels ((trans-to (char from)
             (loop :for to :below (matrix-size matrix)
                   :until (char= char (transition matrix from to))
                   :finally (return to)))
           
           (compute-row (from row)
             (loop :for c :in (nfa-state-inputs state)
                   :for i :from 0
                   :do (setf (aref row i)
                             (trans-to c from)))))
    
    (loop :for frm :below (matrix-size matrix)
          :do (compute-row frm (aref (nfa-state-state-vector state) frm))
          :finally (return state))))

(defun compute-nfa-state (matrix)
  (let ((s (make-nfa-state matrix)))
    (refine-nfa-state matrix s)
    s))

(defvar test (adjacency-matrix 4
                               (list ;  1   2   3   4
                                (list nul #\a nul #\c) ; 1
                                (list eps nul #\b nul) ; 2
                                (list nul #\a nul nul) ; 3
                                (list nul nul #\c nul) ; 4
                                )
                               ))
