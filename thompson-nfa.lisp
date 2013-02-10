;;;; thompson-nfa.lisp
;;;; Copyright (c) 2013 Robert Smith

;;;; Example use:
;;;;
;;;; CL-USER> (regex-match-p `(:concat (:kleene (:either #\a #\b))
;;;;                                   (:option #\c))
;;;;                         "abababbbaabababbabababbabababac")
;;;; T


;;;;;;;;;;;;;;;;;;;;;;; Parsing and Conversion ;;;;;;;;;;;;;;;;;;;;;;;

;;; Regular Expression Grammar:
;;;
;;; <regex> := <char>
;;;          | (:CONCAT <regex> <regex>)
;;;          | (:KLEENE <regex>)
;;;          | (:REPEAT <regex>
;;;          | (:OPTION <regex>)
;;;          | (:EITHER <regex> <regex>)

(defun pre->post (regex)
  (let ((postfix nil))
    (labels ((emit (x)
               (push x postfix))
             
             (generate (regex)
               (if (characterp regex)
                   (emit regex)
                   (let ((length (length regex)))
                     (cond
                       ((= 2 length) (progn
                                       (generate (second regex))
                                       (emit (first regex))))
                       ((= 3 length) (progn
                                       (generate (second regex))
                                       (generate (third regex))
                                       (emit (first regex))))
                       (t (error "invalid regex ~S" regex)))))))
      (generate regex)
      (nreverse postfix))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; State ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *state-counter* 0)

;;; Abstract state
(defstruct state
  (id (incf *state-counter*)))


;;; A vertex with a single non-epsilon edge
(defstruct (wire (:include state))
  char
  out)

;;; A vertex with two epsilon edges
(defstruct (junction (:include state))
  left
  right)

;;; Terminal vertex, indicates a successful match
(defstruct (terminal (:include state)))

(defvar *terminal* (make-terminal)
  "The terminal vertex, indicating a successful match.")

(defconstant +detached+ :detached
  "Denotes an arrow in the NFA pointing to nowhere.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Fragment ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A fragment holds a state and any arrows that need to be connected.
(defstruct fragment
  state
  arrows)

(defun connect (arrows state)
  "Connect all of the arrows ARROWS to the state STATE."
  (dolist (arrow arrows)
    (funcall arrow state)))


;;;;;;;;;;;;;;;;;;;;;;;;;; NFA Construction ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-nfa (instrs)
  "Given a list of (postfix) instructions INSTR, construct an
NFA (represented by a state structure) which simulates the
instructions."
  (let ((frags nil))
    (labels ((push-frag (frag)
               (push frag frags))
             
             (pop-frag ()
               (pop frags))
             
             (process (instr)
               (if (characterp instr)
                   (let ((state (make-wire :char instr
                                           :out +detached+)))
                     (push-frag (make-fragment
                                 :state state
                                 :arrows (list (lambda (st)
                                                 (setf (wire-out state) st))))))
                   (case instr
                     ((:CONCAT)
                      (let* ((e2 (pop-frag))
                             (e1 (pop-frag)))
                        (connect (fragment-arrows e1)
                                 (fragment-state e2))
                        (push-frag (make-fragment
                                    :state (fragment-state e1)
                                    :arrows (fragment-arrows e2)))))
                     
                     ((:KLEENE)
                      (let* ((e (pop-frag))
                             (s (make-junction
                                 :left (fragment-state e)
                                 :right +detached+)))
                        (connect (fragment-arrows e)
                                 s)
                        (push-frag
                         (make-fragment
                          :state s
                          :arrows (list (lambda (st)
                                          (setf (junction-right s) st)))))))
                     
                     ((:REPEAT)
                      (let* ((e (pop-frag))
                             (s (make-junction
                                 :left (fragment-state e)
                                 :right +detached+)))
                        (connect (fragment-arrows e)
                                 s)
                        (push-frag
                         (make-fragment
                          :state (fragment-state e)
                          :arrows (list (lambda (st)
                                          (setf (junction-right s) st)))))))
                     
                     ((:OPTION)
                      (let* ((e (pop-frag))
                             (s (make-junction
                                 :left (fragment-state e)
                                 :right +detached+)))
                        (push-frag
                         (make-fragment
                          :state s
                          :arrows (append
                                   (fragment-arrows e)
                                   (list (lambda (st)
                                           (setf (junction-right s) st))))))))
                     
                     ((:EITHER)
                      (let* ((e2 (pop-frag))
                             (e1 (pop-frag))
                             (s (make-junction
                                 :left (fragment-state e1)
                                 :right (fragment-state e2))))
                        (push-frag (make-fragment
                                    :state s
                                    :arrows (append (fragment-arrows e1)
                                                    (fragment-arrows e2))))))
                     
                     (otherwise (error "invalid instr ~S" instr))))))
      (dolist (instr instrs)
        (process instr))
      
      (assert (= 1 (length frags)))
      
      (let ((final (pop-frag)))
        (connect (fragment-arrows final)
                 *terminal*)
        
        (fragment-state final)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;; NFA Simulation ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simulate-nfa (state string)
  (let ((current-states nil)
        (next-states    nil))
    (labels ((push-state (state)
               (cond
                 ((eql state +detached+)
                  (warn "Found detached arrow..."))
                 
                 ((junction-p state)
                  (progn
                    (push-state (junction-left state))
                    (push-state (junction-right state))))
                 
                 (t (pushnew state next-states :key #'state-id
                                               :test #'=))))
             
             (compute-next-states (char)
               (dolist (state current-states)
                 (when (and (wire-p state)
                            (char= char
                                   (wire-char state)))
                   (push-state (wire-out state)))))
             
             (rotate-states ()
               (shiftf current-states
                       next-states
                       nil))
             
             (match-found-p (states)
               ;; XXX: This does a simple pointer check. This could
               ;; break if copying occurs!
               (and (find *terminal* states :test #'eq)
                    t)))
      
      ;; We use this hack so we do not need two PUSH-STATE functions.
      (push-state state)
      (rotate-states)
      
      (loop :for c :across string
            :do (progn
                  (compute-next-states c)
                  (rotate-states))
            :finally (return (match-found-p current-states))))))

(defun regex-match-p (regex string)
  "Does the string STRING match the regular expression REGEX (in
  S-expression form)?"
  (simulate-nfa (make-nfa (pre->post regex))
                string))
