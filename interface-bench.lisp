(declaim (optimize speed (safety 0) (debug 0)))

(defclass clos-stack () ())
(defgeneric clos-make-stack (impl))
(defgeneric clos-push-stack (impl s x))
(defgeneric clos-pop-stack (impl s))
(defgeneric clos-peek-stack (impl s))

(defclass clos-list-stack (clos-stack) ())
(defmethod clos-make-stack ((impl clos-list-stack))
  nil)
(defmethod clos-push-stack ((impl clos-list-stack) s x)
  (cons x s))
(defmethod clos-pop-stack ((impl clos-list-stack) s)
  (cdr s))
(defmethod clos-peek-stack ((impl clos-list-stack) s)
  (car s))

(defparameter *stk* (make-instance 'clos-list-stack))

;;;;

(define-interface stack ()
  (make-stack (&rest r))
  (push-stack (s x))
  (peek-stack (s))
  (pop-stack (s)))

(define-implementation list-stack (stack)
  :make-stack
  (lambda (&rest r)
    r)

  :push-stack
  (lambda (s x)
    (cons x s))

  :peek-stack
  (lambda (s)
    (car s))
  
  :pop-stack
  (lambda (s)
    (cdr s)))

;;;;

(defun test-clos (trials)
  (declare (type fixnum trials))
  (time
   (let ((s (clos-make-stack *stk*)))
     (dotimes (i trials)
       (declare (type fixnum i))
       (setq s (clos-push-stack *stk* s (random 100)))
       (setq s (clos-pop-stack  *stk* s))))))

(defun test-intf (trials)
  (declare (type fixnum trials))
  (time
   (let ((s (make-stack list-stack)))
     (dotimes (i trials)
       (declare (type fixnum i))
       (setq s (push-stack list-stack s (random 100)))
       (setq s (pop-stack list-stack  s))))))

;;;;;;;;;;;;;;;;;;;;; SBCL "1.1.18.576-5982d4c" ;;;;;;;;;;;;;;;;;;;;;;

;; CL-USER> (test-clos 100000000)
;; Evaluation took:
;;   3.934 seconds of real time
;;   3.951747 seconds of total run time (3.897092 user, 0.054655 system)
;;   [ Run times consist of 0.150 seconds GC time, and 3.802 seconds non-GC time. ]
;;   100.46% CPU
;;   24 lambdas converted
;;   11,015,288,657 processor cycles
;;   7 page faults
;;   1,601,586,992 bytes consed
;; NIL

;; CL-USER> (test-clos 100000000)
;; Evaluation took:
;;   3.854 seconds of real time
;;   3.870247 seconds of total run time (3.838690 user, 0.031557 system)
;;   [ Run times consist of 0.100 seconds GC time, and 3.771 seconds non-GC time. ]
;;   100.42% CPU
;;   10,790,577,826 processor cycles
;;   1,600,025,312 bytes consed  
;; NIL

;; CL-USER> (test-intf 100000000)
;; Evaluation took:
;;   2.412 seconds of real time
;;   2.426226 seconds of total run time (2.400478 user, 0.025748 system)
;;   [ Run times consist of 0.096 seconds GC time, and 2.331 seconds non-GC time. ]
;;   100.58% CPU
;;   6,755,429,028 processor cycles
;;   1,599,995,600 bytes consed
;; NIL

;; CL-USER> (test-intf 100000000)
;; Evaluation took:
;;   2.442 seconds of real time
;;   2.457439 seconds of total run time (2.428560 user, 0.028879 system)
;;   [ Run times consist of 0.107 seconds GC time, and 2.351 seconds non-GC time. ]
;;   100.61% CPU
;;   6,838,983,121 processor cycles
;;   1,599,995,856 bytes consed  
;; NIL


;;;;;;;;;;;;;; CCL "Version 1.9-r15759  (DarwinX8664)" ;;;;;;;;;;;;;;;

;; CL-USER> (test-clos 100000000)
;; (LET ((S (CLOS-MAKE-STACK *STK*))) (DOTIMES (I TRIALS) (DECLARE (TYPE FIXNUM I)) (SETQ S (CLOS-PUSH-STACK *STK* S (RANDOM 100))) (SETQ S (CLOS-POP-STACK *STK* S))))
;; took 7,136,240 microseconds (7.136240 seconds) to run.
;;        439,612 microseconds (0.439612 seconds, 6.16%) of which was spent in GC.
;; During that period, and with 4 available CPU cores,
;;      6,898,516 microseconds (6.898516 seconds) were spent in user mode
;;        269,640 microseconds (0.269640 seconds) were spent in system mode
;;  1,600,000,000 bytes of memory allocated.
;; NIL

;; CL-USER> (test-clos 100000000)
;; (LET ((S (CLOS-MAKE-STACK *STK*))) (DOTIMES (I TRIALS) (DECLARE (TYPE FIXNUM I)) (SETQ S (CLOS-PUSH-STACK *STK* S (RANDOM 100))) (SETQ S (CLOS-POP-STACK *STK* S))))
;; took 7,113,705 microseconds (7.113705 seconds) to run.
;;        441,757 microseconds (0.441757 seconds, 6.21%) of which was spent in GC.
;; During that period, and with 4 available CPU cores,
;;      6,877,457 microseconds (6.877457 seconds) were spent in user mode
;;        269,897 microseconds (0.269897 seconds) were spent in system mode
;;  1,600,000,000 bytes of memory allocated.
;; NIL

;; CL-USER> (test-intf 100000000)
;; (LET ((S (MAKE-STACK LIST-STACK))) (DOTIMES (I TRIALS) (DECLARE (TYPE FIXNUM I)) (SETQ S (PUSH-STACK LIST-STACK S (RANDOM 100))) (SETQ S (POP-STACK LIST-STACK S))))
;; took 5,594,169 microseconds (5.594169 seconds) to run.
;;        431,628 microseconds (0.431628 seconds, 7.72%) of which was spent in GC.
;; During that period, and with 4 available CPU cores,
;;      5,361,139 microseconds (5.361139 seconds) were spent in user mode
;;        268,219 microseconds (0.268219 seconds) were spent in system mode
;;  1,600,000,000 bytes of memory allocated.
;; NIL

;; CL-USER> (test-intf 100000000)
;; (LET ((S (MAKE-STACK LIST-STACK))) (DOTIMES (I TRIALS) (DECLARE (TYPE FIXNUM I)) (SETQ S (PUSH-STACK LIST-STACK S (RANDOM 100))) (SETQ S (POP-STACK LIST-STACK S))))
;; took 5,582,371 microseconds (5.582371 seconds) to run.
;;        429,518 microseconds (0.429518 seconds, 7.69%) of which was spent in GC.
;; During that period, and with 4 available CPU cores,
;;      5,344,141 microseconds (5.344141 seconds) were spent in user mode
;;        271,791 microseconds (0.271791 seconds) were spent in system mode
;;  1,600,000,013 bytes of memory allocated.

