;;; Generate APIs

(defun ensure-list (obj)
  "Make OBJ a list if it already isn't one."
  (if (listp obj)
      obj
      (list obj)))

(defun emit-comment (comment &optional (level 3))
  "Generate a comment string from the comment COMMENT at level LEVEL."
  (check-type level integer)
  (assert (plusp level)
          (level)
          "Comment level must be a positive integer. Given ~S."
          level)
  (concatenate 'string
               (make-string level :initial-element #\;)
               " "
               comment))

(defun emit-function (name args &key documentation)
  (when (and (stringp documentation)
             (string= documentation ""))
    (setf documentation nil))
  (let ((*print-case* :downcase))
    (with-output-to-string (s)
      (pprint 
       `(defun ,name ,args
          ,@(ensure-list documentation))
       s))))

(emit-comment
 "Create a comment containing STRING at depth level LEVEL."
 (string level -> list)
 
 )