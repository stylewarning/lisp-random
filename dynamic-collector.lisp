;;;; dynamic-collector.lisp
;;;; Copyright (c) 2013 Robert Smith

(define-condition messenger (condition)
  ((payload :initarg :payload
            :reader messenger-payload)
   (continuep :initarg :continuep
              :initform t
              :reader messenger-continuep))
  (:documentation "A condition to carry messages between parts of programs."))

(defun collect (data &key return-value
                          (continuep t))
  "Collect the data DATA in a DYNAMIC-COLLECT environment. Optionally
return the value RETURN-VALUE from the form. If CONTINUEP is null,
then collecting will cease."
  (restart-case (signal 'messenger :payload data
                                   :continuep continuep)
    (continue ()
      return-value)))

(defmacro dynamic-collect (&body body)
  "Dynamically collect messages that were signalled during the
execution of BODY. Return a list of messages in the order they were
collected."
  (let ((messages (gensym "MESSAGES-"))
        (block-name (gensym "BLOCK-NAME-")))
    `(let ((,messages nil))
       (block ,block-name
         (handler-bind
             ((messenger (lambda (m)
                           (push (messenger-payload m) ,messages)
                           (if (messenger-continuep m)
                               (invoke-restart 'continue)
                               (return-from ,block-name
                                 (nreverse ,messages))))))
           ,@body)
         (nreverse ,messages)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;; EXAMPLE USAGE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pass-1 (data)
  (unless (integerp data)
    (collect (format nil "Warning: Not an integer: ~S" data))))

(defun pass-2 (data)
  (when (and (listp data)
             (plusp (length data)))
    (unless (symbolp (car data))
      (collect (format nil "Error: A function call needs a symbol ~
                            in the first position, given: ~S"
                       (car data))
        :continuep nil))))

(defun process-messages (messages)
  (if (null messages)
      (format t "No messages.~%")
      (format t "~R message~:P:~%~{  >> ~A~%~}" (length messages) messages)))

(defun main (data)
  (let ((messages (dynamic-collect
                    (pass-1 data)
                    (format t ";;; Done with pass 1.~%")
                    (force-output)
                    (pass-2 data)
                    (format t ";;; Done with pass 2.~%")
                    (force-output))))
    (process-messages messages)))

;; CL-USER> (main 5)
;; ;;; Done with pass 1.
;; ;;; Done with pass 2.
;; No messages.
;; NIL
;; CL-USER> (main :quux)
;; ;;; Done with pass 1.
;; ;;; Done with pass 2.
;; one message:
;;   >> Warning: Not an integer: :QUUX
;; NIL
;; CL-USER> (main '(hello))
;; ;;; Done with pass 1.
;; ;;; Done with pass 2.
;; one message:
;;   >> Warning: Not an integer: (HELLO)
;; NIL
;; CL-USER> (main '(5 hello))
;; ;;; Done with pass 1.
;; two messages:
;;   >> Warning: Not an integer: (5 HELLO)
;;   >> Error: A function call needs a symbol in the first position, given: 5
;; NIL
