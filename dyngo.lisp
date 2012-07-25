;;; Dynamic GO.

;;; Algorithm:
;;; 
;;; 1. Initialize *TAGBODY-ENVIRONMENT* to
;;; 
;;; 2. Enter TAGBODY.
;;;
;;; 3. Collect TAGBODY symbols (the tags), and push them to the top of
;;; *TAGBODY-ENVONMENT*.
;;;
;;; 4. Walk TAGBODY forms.
;;;
;;; 5. If another TAGBODY is found, go to step #1.
;;;
;;; 6. If DYNGO is found, expand DYNGO with the current
;;; *TAGBODY-ENVIRONMENT*.
;;; 
;;; 7. Pop *TAGBODY-ENVIRONMENT*.


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

