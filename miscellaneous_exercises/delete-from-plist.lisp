;;;; delete-from-plist.lisp
;;;; Author: Robert Smith

(defun delete-single-from-plist (plist key-to-remove)
  "Delete the single key KEY-TO-REMOVE from the plist PLIST."
  (flet ((assert-proper-plist (x)
           (assert x () "Expected a proper plist, got ~S" plist))
         (bad-key-p (key)
           (eq key key-to-remove)))
    (cond
      ((null plist) nil)
      ((null (cdr plist)) (assert-proper-plist (cdr plist)))
      ((bad-key-p (car plist)) (cddr plist))
      (t (loop :with start := plist
               :with last-good := plist
               :for the-cons :on (cddr plist) :by #'cddr
               :do (progn
                     (assert-proper-plist (cdr the-cons))
                     (if (bad-key-p (car the-cons))
                         (progn
                           (setf (cddr last-good)
                                 (cddr the-cons))
                           ;; Return early. Unfortunately, this means
                           ;; we are not checking if the entire list
                           ;; is a proper plist.
                           (return start))
                         (setf last-good the-cons)))
               :finally (return start))))))

(defun delete-from-plist (plist &rest keys)
  "Delete all keys and pairs indicated by KEYS from the plist PLIST."
  (labels ((assert-proper-plist (x)
             (assert x () "Expected a proper plist, got ~S" plist))
           (bad-key-p (key)
             (member key keys :test #'eq))
           (find-first ()
             "Find the first cons in PLIST to keep."
             (loop :for the-cons :on plist :by #'cddr
                   :unless (prog1 (bad-key-p (car the-cons))
                             (assert-proper-plist (cdr the-cons)))
                     :do (return the-cons)
                   :finally (return nil))))
    (declare (inline assert-proper-plist
                     bad-key-p
                     find-first))
    ;; Find the first good key and delete any bad key-value pairs
    ;; between it and the start.
    (let ((first (find-first)))
      (unless (eq first plist)
        (setf (cddr plist)
              first))
      
      ;; At this point, we know FIRST points to the first key
      ;; which exists, or NIL.
      (loop :with last-good := first    ; Keep the last good key
            :for the-cons :on (cddr first) :by #'cddr
            :do (progn
                  (assert-proper-plist (cdr the-cons))
                  (if (bad-key-p (car the-cons))
                      (setf (cddr last-good)
                            (cddr the-cons))
                      (setf last-good the-cons)))
            :finally (return first)))))
