;;;; Cycle Detection
;;;; Code copyright (c) 2011 Robert Smith
;;;; Challenge copyright (c) 2011 T. Thain

;;;; CHALLENGE: Given a sequence s = (s1, ..., sn, ...), determine if
;;;; S is cyclical.

(use-package :qtility)

(defun cyclicp (function origin &key (test 'equalp))
  "Compute the phase and period of the function

    g(i) := FUNCTION^i(SEED)

where f^n is n compositions of f. Return two values, the period and
phase respectively. FUNCTION should have a type signature of

    Eq(A) A -> A

where Eq denotes that A has an equality predicate TEST."
  (flet ((f (x) (funcall function x))
         (f^2 (x) (funcall (compose function function) x)))
    (let (start-of-cycle
          coinciding-position
          phase
          period)
      
      ;; Find when the tortoise and the hare land on the same value,
      ;; COINCIDING-POSITION.
      (loop
        :for tortoise := (f origin) :then (f tortoise)
        :for hare := (f^2 origin) :then (f^2 hare)
        :until (funcall test tortoise hare)
        :do (do-nothing)
        :finally (setf coinciding-position hare))
      
      ;; Calculate the "phase" of the cycle. The phase is the distance
      ;; between the COINCIDING-POSITION and the ORIGIN. To
      ;; calculate the distance, we have two tortoises walk in
      ;; parallel, one at the starting position, the other where the
      ;; hare stopped. We wait until these tortoises land in the same
      ;; place.
      (loop
        :for steps := 0 :then (1+ steps)
        :for tortoise-1 := origin :then (f tortoise-1)
        :for tortoise-2 := coinciding-position :then (f tortoise-2)
        :until (funcall test tortoise-1 tortoise-2)
        :do (do-nothing)
        :finally (setf phase          steps
                       start-of-cycle tortoise-1))
      
      ;; Calculate the period of the cycle. We do this by setting a
      ;; tortoise at the start of the cycle found previously,
      ;; START-OF-CYCLE, and let it step until it reaches the same
      ;; position.
      (loop
        :for steps := 1 :then (1+ steps)
        :for tortoise := (f start-of-cycle) :then (f tortoise)
        :until (funcall test start-of-cycle tortoise)
        :do (do-nothing)
        :finally (setf period steps))
      
      (values period phase))))

(defun cyclic-list-p (list)
  (flet ((safe-cdr (lst)
           (if (null lst)
               (throw :finite-list nil)
               (cdr lst))))
    (when (consp list)
      (boolify (catch :finite-list (cyclicp #'safe-cdr list))))))

(defun test-this-out ()
  (assert (eq nil (cyclic-list-p '(1 2 3 4 5))))
  (assert (eq t (cyclic-list-p (cycle '(1 2 3 4 5)))))
  (assert (eq nil (cyclic-list-p '())))
  (assert (eq nil (cyclic-list-p 5)))
  (assert (eq t (cyclic-list-p (append '(1 2 3 4 5)
                                       (cycle (list 8 2 4 7 0 1 1 3))))))
  t)