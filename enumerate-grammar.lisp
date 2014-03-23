;;;; enumerate-grammar.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith
;;;;
;;;; Enumerate all strings in context-free grammars.

(ql:quickload :cl-algebraic-data-type)

(load "stack-queue.lisp")

(setq *print-circle* t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun kw (x)
    (intern (symbol-name x) :keyword))
)

(adt:defdata sym
  (terminal     string)
  (non-terminal keyword))

(defstruct (grammar (:constructor %make-grammar))
  root
  rules)

(defun make-grammar (root &rest name-rule-list)
  (loop :with table := (make-hash-table)
        :for (name rule) :on name-rule-list :by #'cddr
        :do (setf (gethash name table) rule)
        :finally (return (%make-grammar :root (adt:match sym root
                                                ((non-terminal x) x)
                                                (_ (error "Invalid root.")))
                                        :rules table))))

(defmacro with-non-terminals ((&rest symbols) &body body)
  `(let ,(loop :for symbol :in symbols
               :collect `(,symbol (non-terminal ,(kw symbol))))
     ,@body))

(defun alternates (&rest things)
  (loop :for thing :in things
        :collect (if (listp thing)
                     thing
                     (list thing))))

(defparameter *digit-grammar*
  (with-non-terminals (digits more)
    (make-grammar digits
      :digits (alternates (terminal "1") (terminal "2") more)
      :more (alternates (terminal "3") (terminal "4")))))

(defparameter *simple-grammar*
  (let ((one (terminal "1")))
    (with-non-terminals (digits)
      (make-grammar digits
        :digits (alternates one (list one digits))))))

(defparameter *binary-grammar*
  (let ((zero (terminal "0"))
        (one  (terminal "1")))
    (with-non-terminals (num digits digit)
      (make-grammar num
        :num (alternates zero (list one) (list one digits))
        :digits (alternates digit (list digit digits))
        :digit (alternates zero one)))))

(defparameter *simple-arithmetic-grammar*
  (with-non-terminals (expr add term digit)
    (make-grammar expr
      :expr (alternates add)
      :add  (alternates term
                        (list add (terminal "+") term))
      :term (alternates digit
                        (list (terminal "(") expr (terminal ")")))
      :digit (loop :for i :below 2
                   :collect (list (terminal (prin1-to-string i)))))))

(defparameter *arithmetic-grammar*
  (with-non-terminals (expr add mul term digit number)
    (make-grammar expr
      :expr (alternates add)
      :add  (alternates mul
                        (list add (terminal "+") mul))
      :mul  (alternates term
                        (list mul (terminal "*") term))
      :term (alternates number
                        (list (terminal "(") expr (terminal ")")))
      :number (alternates digit (list digit number))
      :digit (loop :for i :below 10
                   :collect (list (terminal (prin1-to-string i)))))))

(defun combinations (lists)
  (if (null (cdr lists))
      (mapcar #'list (car lists))
      (mapcan (lambda (item)
                (mapcar (lambda (combo)
                          (cons item combo))
                        (combinations (cdr lists))))
              (car lists))))

(defun flatten (list)
  (loop :for x :in list
        :if (listp x)
          :append x
        :else
          :collect x))

(defun enumerate (grammar &optional (limit 10))
  (let ((table (grammar-rules grammar))
        (todo (make-queue)))
    (labels ((print-alternate (alternate)
               (write-string "==> ")
               (dolist (terminal alternate)
                 (adt:match sym terminal
                   ((terminal x) (write-string x))
                   (_ (error "Not a terminal!"))))
               (terpri))
             
             (output-or-queue (alternate)
               (if (every #'terminal-p alternate)
                   (print-alternate alternate)
                   (enqueue todo alternate)))

             ;; Takes a list of terminals and nonterminals, generates
             ;; a list appropriate for COMBINATIONS, and computes a
             ;; list of all combinations.
             (compute-combinations (alternate)
               (let ((thing (mapcar (lambda (sym)
                                      (adt:match sym sym
                                        ((terminal _) (list sym))
                                        ((non-terminal x) (copy-list (gethash x table)))))
                                    alternate)))
                 
                 (mapcar #'flatten (combinations thing)))))

      ;; Enqueue initial alternates.
      (dolist (alternate (gethash (grammar-root grammar) table))
        (enqueue todo alternate))

      ;; Generate all strings.
      (loop :repeat limit
            :until (queue-empty-p todo)
            :do (mapc #'output-or-queue
                      (compute-combinations (dequeue todo)))))))
