;;;; dyngo.lisp
;;;; Copyright (c) 2012 Robert Smith

;;; Algorithm
;;; =========
;;; 
;;; 1, Initialize *TAGBODY-ENVIRONMENT* to NIL.
;;; 
;;; 2. Enter TAGBODY. Push a new frame onto *TAGBODY-ENVIRONMENT*.
;;;
;;; 3. Collect TAGBODY symbols (the tags), and push them to current
;;;    frame.
;;;
;;; 4. Walk TAGBODY forms.
;;;
;;;   4a. If another TAGBODY is found, go to step #2.
;;;
;;;   4b. If DYNGO is found, expand DYNGO with the current
;;;       *TAGBODY-ENVIRONMENT*.
;;; 
;;; 5. Pop *TAGBODY-ENVIRONMENT*.
;;; 
;;; 
;;; The expansion of a (DYNGO X) form is a (CASE X ...) form where
;;; each case of of the form (<tag> (go <tag)), where <tag> is a tag
;;; from the *TAGBODY-ENVIRONMENT*.
;;; 
;;; If SPACE > SPEED, then DYNGO can "redirect" to a single giant jump
;;; table, as opposed to embedding the table at each DYNGO
;;; invocation. However, the number of tables, even for SPACE > SPEED,
;;; is generally linear in the number of nested TAGBODY forms.


;;;;;;;;;;;;;;;;;;;;;;;; TAGBODY ENVIRONMENT ;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *tagbody-environment* nil)

(defun push-frame ()
  "Push a new empty frame."
  (push nil *tagbody-environment*))

(defun pop-frame ()
  "Pop a frame."
  (pop *tagbody-environment*))

(defun push-tag (tag)
  "Push a new tag TAG onto the current frame."
  (pushnew tag (first *tagbody-environment*)))

(defun tagbody-environment-tags ()
  "A list of all tags in the tagbody environment."
  (remove-duplicates (loop :for frame :in *tagbody-environment*
                           :append frame)))

;;;;;;;;;;;;;;;;;;;;;;; JUMP TABLE GENERATION ;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-jump-table (expr &key (errorp t))
  "Generate a jump table, branching on EXPR."
  (labels ((make-case (tag)
             `((,tag) (go ,tag))))
    (let ((tag (gensym "TAG-")))
      `(let ((,tag ,expr))
         (case ,tag
           ,@(mapcar #'make-case (tagbody-environment-tags))
           ,@(when errorp
               `((otherwise (error "Unknown tag ~S." ,tag)))))))))


;;;;;;;;;;;;;;;;;;;;;;; DYNAMIC TAGBODY/DYNGO ;;;;;;;;;;;;;;;;;;;;;;;;

#+#:ignore
(defmacro dyngo (tag)
  (generate-jump-table tag))

