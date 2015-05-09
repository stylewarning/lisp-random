;;;; illogical-pathnames.lisp
;;;;
;;;; Version 1.0

;;;; DEPRECATED DEPRECATED DEPRECATED DEPRECATED DEPRECATED
;;;;
;;;; Use ILLOGICAL-PATHNAMES instead. It supersedes this.
;;;;
;;;; https://bitbucket.org/tarballs_are_good/illogical-pathnames
;;;;
;;;; DEPRECATED DEPRECATED DEPRECATED DEPRECATED DEPRECATED

;;; Copyright (c) 2015, Robert Smith <quad@symbo1ics.com>
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;; 1. Redistributions of source code must retain the above copyright
;;; notice, this list of conditions and the following disclaimer.
;;;
;;; 2. Redistributions in binary form must reproduce the above
;;; copyright notice, this list of conditions and the following
;;; disclaimer in the documentation and/or other materials provided
;;; with the distribution.
;;;
;;; 3. Neither the name of the copyright holder nor the names of its
;;; contributors may be used to endorse or promote products derived
;;; from this software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;; CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
;;; BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
;;; ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
;;; TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
;;; THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.

(defpackage #:illogical-pathnames
  (:use #:cl)
  (:nicknames #:ipath)
  (:export
   #:illogical-host                     ; TYPE
   #:illogical-host-translation         ; FUNCTION, SETF
   #:define-illogical-host              ; MACRO

   #:illogical-pathname                 ; TYPE (STRUCT)
   #:illogical-pathname-p               ; FUNCTION (PREDICATE)
   #:illogical-pathname-name            ; FUNCTION
   #:illogical-pathname-type            ; FUNCTION
   #:illogical-pathname-directory       ; FUNCTION
   #:illogical-pathname-host            ; FUNCTION

   #:translate-illogical-pathname       ; FUNCTION

   #:enable-illogical-pathname-syntax   ; FUNCTION (affects reader)
   #:disable-illogical-pathname-syntax  ; FUNCTION (affects reader)
   ))

(in-package #:illogical-pathnames)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; START OF CL-FAD PORTION ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The following is a reproduction of a minimum number of pathname
;;; utilities provided by CL-FAD. Most of them are trivial, but are
;;; nonetheless copied from CL-FAD.
;;;
;;; We opt to not rely on CL-FAD for bootstrapping reasons. Things li
;;;
;;; Below is the copyright notice and license agreement as required by
;;; CL-FAD.

;;; Copyright (c) 2004, Peter Seibel.  All rights reserved.
;;; Copyright (c) 2004-2010, Dr. Edmund Weitz.  All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


(defun component-present-p (value)
  "Helper function for DIRECTORY-PATHNAME-P which checks whether VALUE
   is neither NIL nor the keyword :UNSPECIFIC."
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p (pathspec)
  "Returns NIL if PATHSPEC \(a pathname designator) does not designate
a directory, PATHSPEC otherwise.  It is irrelevant whether file or
directory designated by PATHSPEC does actually exist."
  (and
    (not (component-present-p (pathname-name pathspec)))
    (not (component-present-p (pathname-type pathspec)))
    pathspec))

(defun pathname-absolute-p (a)
  "Returns true if A is an absolute pathname.

This simply tests if A's directory list starts with :ABSOLUTE"
  (eql :absolute (first (pathname-directory (pathname a)))))

(defun pathname-as-file (pathspec)
  "Converts the non-wild pathname designator PATHSPEC to file form."
  (let ((pathname (pathname pathspec)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (cond ((directory-pathname-p pathspec)
           (let* ((directory (pathname-directory pathname))
                  (name-and-type (pathname (first (last directory)))))
             (make-pathname :directory (butlast directory)
                            :name (pathname-name name-and-type)
                            :type (pathname-type name-and-type)
                            :defaults pathname)))
          (t pathname))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; END OF CL-FAD PORTION ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Illogical Hosts

(deftype illogical-host ()
  "The type of an illogical host object."
  'symbol)

(defvar *illogical-hosts* (make-hash-table :test 'eql)
  "Association between ILLOGICAL-HOSTs and their pathname translation.")

(defun illogical-host-translation (illogical-host)
  "Translate the illogical host ILLOGICAL-HOST to its defined absolute directory."
  (values (gethash illogical-host *illogical-hosts*)))

(defun (setf illogical-host-translation) (directory illogical-host)
  (check-type illogical-host illogical-host)
  (assert (directory-pathname-p directory)
          (directory)
          "~S doesn't look like a directory."
          directory)
  (assert (pathname-absolute-p directory)
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

(defmethod make-load-form ((self illogical-pathname) &optional environment)
  (make-load-form-saving-slots self
                               :slot-names '(host directory name type)
                               :environment environment))

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
                               (pathname-as-file filename))))
        (assert (null (pathname-directory file-pathname)))
        `(translate-illogical-pathname
          ,(make-illogical-pathname
            :host host
            :directory (copy-list directory)
            :name (pathname-name file-pathname)
            :type (pathname-type file-pathname)))))))

(defun |new-#P-reader| (stream subchar arg)
  (let ((char (peek-char nil stream)))
    (if (char= #\( char)
        (funcall #'|illogical-#P-reader| stream subchar arg)
        (funcall *common-lisp-sharp-p* stream subchar arg))))

(defun enable-illogical-pathname-syntax ()
  "Enable illogical pathname syntax.

    #P\"...\"  ; traditional pathname syntax

    #P(<illogical-host> (<directory name>*) <filename>?)
"
  (unless (boundp '*common-lisp-sharp-p*)
    (setf *common-lisp-sharp-p* (get-dispatch-macro-character #\# #\P)))

  (set-dispatch-macro-character #\# #\P #'|new-#P-reader|))

(defun disable-illogical-pathname-syntax ()
  "Disable illogical pathname syntax."
  (when (boundp '*common-lisp-sharp-p*)
    (set-dispatch-macro-character #\# #\P *common-lisp-sharp-p*)
    t))

(enable-illogical-pathname-syntax)

;;; Example usage.
;;; CL-USER> (load "illogical-pathnames.lisp")
;;; ; Loading text file illogical-pathnames.lisp
;;; #P"illogical-pathnames.lisp"
;;; CL-USER> #P"/foo/bar"
;;; #P"/foo/bar"
;;; CL-USER> (ipath:define-illogical-host :home "/home/me/")
;;; :HOME
;;; CL-USER> #P(:home ("Scratch") "new.txt")
;;; #P"/home/me/Scratch/new.txt"
;;; CL-USER> '#P(:home ("Scratch") "new.txt")
;;; (ILLOGICAL-PATHNAMES:TRANSLATE-ILLOGICAL-PATHNAME
;;;   #S(ILLOGICAL-PATHNAMES:ILLOGICAL-PATHNAME
;;;       :HOST :HOME
;;;       :DIRECTORY ("Scratch")
;;;       :NAME "new"
;;;       :TYPE "txt"))
;;; CL-USER> (ipath:define-illogical-host :scratch #P(:home ("Scratch")))
;;; :SCRATCH
;;; CL-USER> (with-open-file (s #P(:scratch nil "test.txt") :direction ':output
;;;                                                         :if-does-not-exist ':create)
;;;            (write-string "testing, 1 2 3" s))
;;; "testing, 1 2 3"
;;; CL-USER> (with-open-file (s #P(:scratch nil "test.txt") :direction ':input)
;;;            (read-line s))
;;; "testing, 1 2 3"
;;; T
