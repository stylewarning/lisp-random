;;;; illogical-pathnames.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

(ql:quickload :cl-fad)

(deftype illogical-host ()
  'symbol)

(defvar *illogical-hosts* (make-hash-table :test 'eql)
  "Association between ILLOGICAL-HOSTs and their pathname translation.")

(defun illogical-host-translation (illogical-host)
  "Translate the illogical host ILLOGICAL-HOST to its defined absolute directory."
  (values (gethash illogical-host *illogical-hosts*)))

(defun (setf illogical-host-translation) (directory illogical-host)
  (check-type illogical-host illogical-host)
  (assert (cl-fad:directory-pathname-p directory)
          (directory)
          "~S doesn't look like a directory."
          directory)
  (assert (cl-fad:pathname-absolute-p directory)
          (directory)
          "~S isn't an absolute path."
          directory)
  (setf (gethash illogical-host *illogical-hosts*) directory))

(defmacro define-illogical-host (host directory &optional documentation)
  "Define the illogical host HOST to the absolute directory DIRECTORY."
  (declare (ignore documentation))
  (check-type host illogical-host)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (illogical-host-translation ,host) ,directory)
     ,host))

(defstruct illogical-pathname
  host
  directory
  name
  type)

(defun illogical-pathname-relative-pathname (illogical-pathname)
  "Convert the illogical pathname ILLOGICAL-PATHNAME to its representative relative pathname."
  (make-pathname
   :directory (list* :relative (illogical-pathname-directory
                                illogical-pathname))
   :name (illogical-pathname-name illogical-pathname)
   :type (illogical-pathname-type illogical-pathname)))

(defun translate-illogical-pathname (illogical-pathname)
  "Translate the illogical pathname ILLOGICAL-PATHNAME to its equivalent absolute pathname."
  (check-type illogical-pathname illogical-pathname)
  (let* ((host (illogical-pathname-host illogical-pathname))
         (host-translation (illogical-host-translation host)))
    (when (null host-translation)
      (error "No translation for the illogical host ~S" host))
    
    (merge-pathnames (illogical-pathname-relative-pathname illogical-pathname)
                     host-translation)))

(defvar *common-lisp-sharp-p*)

(defun |illogical-#P-reader| (stream subchar arg)
  "Reader for illogical pathnames. Returns an illogical pathname."
  (declare (ignore subchar arg))
  (let* ((*read-eval* nil)
         (specifier (read stream t)))
    (destructuring-bind (host directory &optional filename)
        specifier
      (check-type host illogical-host)
      (check-type filename (or null string))
      (assert (listp directory))
      (assert (every #'stringp directory))
      (let ((file-pathname (if (null filename)
                               (make-pathname)
                               (cl-fad:pathname-as-file filename))))
        (assert (null (pathname-directory file-pathname)))
        (make-illogical-pathname
         :host host
         :directory (copy-list directory)
         :name (pathname-name file-pathname)
         :type (pathname-type file-pathname))))))

(defun |new-#P-reader| (stream subchar arg)
  (let ((char (peek-char nil stream)))
    (if (char= #\( char)
        (funcall #'|illogical-#P-reader| stream subchar arg)
        (funcall *common-lisp-sharp-p* stream subchar arg))))

(defun set-illogical-pathname-syntax ()
  "Enable illogical pathname syntax.

    #P\"...\"  ; traditional pathname syntax

    #P(<illogical-host> (<directory name>*) <filename>?)
"
  (unless (boundp '*common-lisp-sharp-p*)
    (setf *common-lisp-sharp-p* (get-dispatch-macro-character #\# #\P)))
  
  (set-dispatch-macro-character #\# #\P #'|new-#P-reader|))


;;; Example usage.
;;;
;;; CL-USER> (set-illogical-pathname-syntax)
;;; T
;;; CL-USER> #P"/foo/bar"
;;; #P"/foo/bar"
;;; CL-USER> (define-illogical-host :home "/Users/quad/")
;;; :HOME
;;; CL-USER> #P(:home ("Documents"))
;;; #S(ILLOGICAL-PATHNAME
;;;    :HOST :HOME
;;;    :DIRECTORY ("Documents")
;;;    :NAME NIL
;;;    :TYPE NIL)
;;; CL-USER> (translate-illogical-pathname *)
;;; #P"/Users/quad/Documents/"
;;; CL-USER> #P(:home ("Documents") "file.ext")
;;; #S(ILLOGICAL-PATHNAME
;;;    :HOST :HOME
;;;    :DIRECTORY ("Documents")
;;;    :NAME "file"
;;;    :TYPE "ext")
;;; CL-USER> (translate-illogical-pathname *)
;;; #P"/Users/quad/Documents/file.ext"
