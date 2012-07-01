;;; Global, non-EQL constants
;;; Suggestion by Paul Khuong

(defglobal **foo** (make-foo))
(define-symbol-macro +foo+ (load-time-value (the foo **foo**)))

