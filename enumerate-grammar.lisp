;;;; enumerate-grammar.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith
;;;;
;;;; Enumerate all sentences in a context-free grammar.
;;;;
;;;;
;;;; OVERVIEW
;;;;
;;;; This was a late-night weekend first-cut at the challenge set
;;;; forth by Luke Palmer to generate all sentences of a context-free
;;;; (CF) language. His post is at
;;;;
;;;;  http://lukepalmer.wordpress.com/2008/05/02/enumerating-a-context-free-language/
;;;;
;;;; Currently, this is very inefficient. It conses up around 100 GB
;;;; of memory for a 4000 expansion limit (*not* sentence generation
;;;; limit!) of the arithmetic grammar. The order in which things are
;;;; generated seems to be correct (and (probably) easy to prove
;;;; correct), but it doesn't produce the most dazzling outputs. If
;;;; the traversal were better, for some definition of better, this
;;;; could be a lot produce prettier output. It can also probably be
;;;; made more efficient in time and memory significantly.
;;;;
;;;;
;;;; STRATEGY
;;;;
;;;; This is a very simple-minded approach to CF sentence generation.
;;;;
;;;; First, the CF grammar is generated. I originally produced a graph
;;;; representing the grammar which is what is constructed in Palmer's
;;;; Haskell code lazily. My graph took advantage of the ability to
;;;; have cyclic data structures in Lisp. This approach didn't
;;;; immediately work out well, and it was pretty difficult to debug,
;;;; so I abandonded it.
;;;;
;;;; The next approach I had was just to map in a table names of
;;;; non-terminal to lists of alternations, which simply reference the
;;;; non-terminals by name. The aforementioned graph can be
;;;; constructed by "patching" this table up in a post-processing
;;;; step. This turned out to be pretty simple and easy to debug.
;;;;
;;;; Terminals and non-terminals are represented by the SYM algebraic
;;;; data type, which allows for a TERMINAL string or a NON-TERMINAL
;;;; reference. I originally went gung-ho on getting everything
;;;; explicitly type-safe, but then it turned out to be simpler in the
;;;; end to just use lists for, specifically, alternations and
;;;; combinations. The haphazard use of types ended up biting me in
;;;; the behind when I started getting list-vertigo, which you'll see
;;;; below in the tons of flattening and consing.
;;;;
;;;; The actual approach to generation is simple. I do a breadth-first
;;;; algorithm:
;;;;
;;;;    1. Enqueue root alternations.
;;;;
;;;;    2. Pop the next alternation X.
;;;;
;;;;    3. Expand out all combinations of X.
;;;;
;;;;        a. Identify all non-terminals in X and look up their
;;;;        definition. Call these "sub-alternations".
;;;;
;;;;        b. For each combination of sub-alternations, record an
;;;;        expanded alternation.
;;;;
;;;;    4. For each combination, output if it is a sentence, or add to
;;;;    the queue to be expanded further.
;;;;
;;;;    5. If the queue is not empty, go bto step 2. Otherwise, the
;;;;    language has been fully enumerated.

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
  "Create a new grammar with the root non-terminal ROOT, with plist of keyword name and alternation pairs NAME-RULE-LIST."
  (loop :with table := (make-hash-table)
        :for (name rule) :on name-rule-list :by #'cddr
        :do (setf (gethash name table) rule)
        :finally (return (%make-grammar :root (adt:match sym root
                                                ((non-terminal x) x)
                                                (_ (error "Invalid root.")))
                                        :rules table))))

(defmacro with-non-terminals ((&rest symbols) &body body)
  "Lexically bind to SYMBOLS fresh non-terminals refering to the same name as each SYMBOL."
  `(let ,(loop :for symbol :in symbols
               :collect `(,symbol (non-terminal ,(kw symbol))))
     ,@body))

(defun alternates (&rest things)
  "Create a list of alternates specified by THINGS. The elements of THINGS should either be non-terminals, terminals, or a list of non-terminals and terminals.
"
  (loop :for thing :in things
        :collect (if (listp thing)
                     thing
                     (list thing))))

(defun combinations (lists)
  "Given a list of lists LISTS, produce all possible combinations of elements, picking once from each list."
  (if (null (cdr lists))
      (mapcar #'list (car lists))
      (mapcan (lambda (item)
                (mapcar (lambda (combo)
                          (cons item combo))
                        (combinations (cdr lists))))
              (car lists))))

(defun flatten (list)
  "Flatten the list LIST one level."
  (loop :for x :in list
        :if (listp x)
          :append x
        :else
          :collect x))

(defun sentencep (x)
  "Does A represent a sentence?"
  (and (listp x)
       (every #'terminal-p x)))

(defun enumerate (grammar &optional (limit 10))
  "Enumerate all sentences of the context-free language accepted by the grammar GRAMMAR. Limit the number of expansion iterations to LIMIT."
  (let ((table (grammar-rules grammar))
        (todo (make-queue)))
    (labels ((print-alternate (alternate)
               "Print a sentence specified by the list of terminal symbols ALTERNATE."
               (write-string "==> ")
               (dolist (terminal alternate)
                 (adt:match sym terminal
                   ((terminal x) (write-string x))
                   (_ (error "Not a terminal!"))))
               (terpri))
             
             ;;
             (output-or-queue (alternate)
               "If ALTERNATE is a (terminal) sentence in the language specified by GRAMMAR, then output it. Otherwise queue it up for further expansion."
               (if (sentencep alternate)
                   (print-alternate alternate)
                   (enqueue todo alternate)))

             (compute-combinations (alternate)
               "Takes a list of terminals and non-terminals ALTERNATE, and \"expands\" all combinations of the non-terminals in the list. The result is a list of the Cartesian product of all possible non-terminal expansions."
               (let ((thing (mapcar (lambda (sym)
                                      (adt:match sym sym
                                        ((terminal _) (list sym))
                                        ((non-terminal x) (copy-list (gethash x table)))))
                                    alternate)))
                 
                 (mapcar #'flatten (combinations thing)))))

      ;; Enqueue initial alternates.
      (dolist (alternate (gethash (grammar-root grammar) table))
        (enqueue todo alternate))

      ;; Generate strings, up to the specified limit or until the language has been exhausted.
      (loop :repeat limit
            :until (queue-empty-p todo)
            :do (mapc #'output-or-queue
                      (compute-combinations (dequeue todo)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Examples ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *digit-grammar*
  (with-non-terminals (digits more)
    (make-grammar digits
      :digits (alternates (terminal "1") (terminal "2") more)
      :more (alternates (terminal "3") (terminal "4")))))

;; CL-USER> (enumerate *digit-grammar* 100)
;; ==> 1
;; ==> 2
;; ==> 3
;; ==> 4


(defparameter *simple-grammar*
  (let ((one (terminal "1")))
    (with-non-terminals (digits)
      (make-grammar digits
        :digits (alternates one (list one digits))))))

;; CL-USER> (enumerate *simple-grammar* 10)
;; ==> 1
;; ==> 11
;; ==> 111
;; ==> 1111
;; ==> 11111
;; ==> 111111
;; ==> 1111111
;; ==> 11111111
;; ==> 111111111
;; ==> 1111111111

(defparameter *binary-grammar*
  (let ((zero (terminal "0"))
        (one  (terminal "1")))
    (with-non-terminals (num digits digit)
      (make-grammar num
        :num (alternates zero (list one) (list one digits))
        :digits (alternates digit (list digit digits))
        :digit (alternates zero one)))))

;; CL-USER> (enumerate *binary-grammar* 10)
;; ==> 0
;; ==> 1
;; ==> 10
;; ==> 11
;; ==> 100
;; ==> 101
;; ==> 110
;; ==> 111
;; ==> 1000
;; ==> 1001


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

;; CL-USER> (enumerate *simple-arithmetic-grammar* 1000)
;; ==> 0
;; ==> 1
;; ==> 0+0
;; ==> 1+0
;; ==> 0+1
;; ==> 1+1
;; ==> 0+0+0
;; ==> 1+0+0
;; ==> 0+1+0
;; ...
;; ==> 1+0+(0)
;; ==> 1+0+(1)
;; ==> 0+1+(0)
;; ...
;; ==> 0+1+1+(1)
;; ==> 1+1+1+(0)
;; ==> 1+1+1+(1)
;; ==> (0+0)
;; ==> (1+0)
;; ==> (0+1)
;; ...
;; ==> (1)+(0)
;; ==> (0)+(1)
;; ==> (1)+(1)
;; ==> (0)+(0+0)
;; ==> (0)+(1+0)
;; ==> (1)+(1+1)


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

;; This output is rather boring for a long time. Starts off with 0 to
;; 9, continues with 00 to 99, continues with 0*0 to 9*9, then the
;; same for double digits, etc.
