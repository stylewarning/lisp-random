;;;; doc.lisp
;;;;
;;;; Copyright (c) 2020 Robert Smith

(progn
  (function update
            (:description "Notify progress bar about step completion."
             :arguments-and-values ((unit-count "How many steps has been finished?")
                                    (progress-bar "Instance of progress-bar. Usually should be left with default (namely: *progress-bar*)."))
             :thread-safety "This function is thread safe.")))



(defvar *doc-parsers* '((cl:defun . parse-defun))
  "A table mapping symbols to functions, where the functions take two values:

    1. The definiting form.
    2. A list of options.

and return three values:

    1. A setter form for the documentation.
    2. A list of forms to place before the definition.
    3. A list of forms to place after the definition.
"
  )

(defun parse-defun (defun-form options)
  (assert (eq 'defun (first defun-form)))
  (let* ((marker (gensym))
         (name (second defun-form))
         (args (third defun-form))
         (pre  nil)
         (post nil)
         ;;
         (overview    (getf options ':overview    ""))
         (inline      (getf options ':inline      ':no))
         (returns     (getf options ':returns     marker))
         (thread-safe (getf options ':thread-safe nil))
         ;;
         (types nil)
         (return-type nil))
    (flet ((pushdoc (str &rest args)
             (setf overview (concatenate 'string
                                         overview
                                         (format nil "~2%")
                                         (apply #'format nil str args))))
           (arg-types ()
             ;; FIXME: Properly handle &optional args.
             (loop :for arg :in args
                   :for type := (member arg types :key #'car)
                   :if (null type)
                     :collect (list arg 't)
                   :else
                     :collect (list arg (cdar type)))))
      ;; Handle arguments.
      (pushdoc "Args:")
      (loop :for (key val) :on options :by #'cddr
            :when (eq key ':arg)
              :do (destructuring-bind (arg-name arg-type arg-str) val
                    (pushdoc "  ~S [~S]: ~A" arg-name arg-type arg-str)
                    (push (cons arg-name arg-type) types)))
      
      ;; Handle return type.
      (unless (eq marker returns)
        (pushdoc "Returns: ~S" returns)
        (setf return-type (list returns)))

      ;; Handle inlining.
      (ecase inline
        (:no
         nil)
        (:yes
         (push `(declaim (inline ,name)) pre)
         (pushdoc "This function is inlined."))
        (:maybe
         (push `(declaim (inline ,name)) pre)
         (push `(declaim (notinline ,name)) post)
         (pushdoc "This function is inlineable.")))

      ;; Handle thread safety.
      (case thread-safe
        (:yes (pushdoc "This function is thread safe."))
        (:no (pushdoc "This function is NOT thread safe.")))

      ;; Form the type declaration
      (when (or types return-type)
        (push `(declaim (ftype (function ,(arg-types) ,@return-type) ,name))
              pre))
      ;; Return everything
      (values `(setf (documentation ',name 'function) ,overview)
              pre
              post))))

;;; Options:
;;;
;;;     :overview <string>
;;;     :inline {:yes, :maybe, :no}
;;;     :arg (<sym> <type> <string>)
;;;     :returns <type>
;;;     :thread-safe {:yes, :no, nil)

(defun at-reader (stream char)
  ;; @(opts...) form
  (declare (ignore char))
  (let ((options (read stream))
        (form    (read stream)))
    (unless (consp form)
      (error "FORM must be a defining form"))
    (let ((parser (cdr (assoc (first form) *doc-parsers*))))
      (when (null parser)
        (error "No doc parser found for ~S" (first form)))
      (multiple-value-bind (doc-setter pre post)
          (funcall parser form options)
        `(progn
           ,@pre
           ,form
           ,doc-setter
           ,@post)))))

(set-macro-character #\@ 'at-reader)

;;; Example

@(:overview "Extracts the polar radius from a rectangular coordinate."
  :arg (x single-float "The X coordinate of point.")
  :arg (y single-float "The Y coordinate of point.")
  :returns single-float
  :thread-safe :yes
  :inline :maybe)
(defun hypot (x y)
  (sqrt (+ (* x x) (* y y))))

;;; The docstring will be:

#+#:ignore
"Extracts the polar radius from a rectangular coordinate.

Args:

  X [SINGLE-FLOAT]: The X coordinate of point.

  Y [SINGLE-FLOAT]: The Y coordinate of point.

Returns: SINGLE-FLOAT

This function is inlineable.

This function is thread safe."

;;; The macroexpansion will be:

#+#:ignore
(PROGN
 (DECLAIM
  (FTYPE (FUNCTION ((X SINGLE-FLOAT) (Y SINGLE-FLOAT)) SINGLE-FLOAT) HYPOT))
 (DECLAIM (INLINE HYPOT))
 (DEFUN HYPOT (X Y) (SQRT (+ (* X X) (* Y Y))))
 (SETF (DOCUMENTATION 'HYPOT 'FUNCTION)
         "Extracts the polar radius from a rectangular coordinate.

Args:

  X [SINGLE-FLOAT]: The X coordinate of point.

  Y [SINGLE-FLOAT]: The Y coordinate of point.

Returns: SINGLE-FLOAT

This function is inlineable.

This function is thread safe.")
 (DECLAIM (NOTINLINE HYPOT)))

