;[16:11.43]  <phoe_krk> let's assume I have three directly nested do loops
;[16:11.55]  <phoe_krk> with vars x y z
;[16:12.05]  <phoe_krk> I have to iterate through all the combinations
;[16:12.41]  <phoe_krk> is there any sort of (do-all ((x 0 10) (y 0 10) (z 0 10)) (...)) macro that would iterate through all x*y*z combinations in any order?

(defmacro do-all* (var-ranges &body body)
  (if (null var-ranges)
      `(progn ,@body)
      (destructuring-bind (var lower upper)
          (first var-ranges)
        `(loop :for ,var :from ,lower :to ,upper :do
          (do-all ,(rest var-ranges)
            ,@body)))))

(defmacro do-all (var-ranges &body body)
  (let* ((num-vars (length var-ranges))
         (vars (mapcar #'first var-ranges))
         (lowers (map-into (make-list num-vars :initial-element "LOWER-") #'gensym))
         (uppers (map-into (make-list num-vars :initial-element "UPPER-") #'gensym))
         (let-bindings (loop :for (lower-val upper-val) :in (mapcar #'cdr var-ranges)
                             :for lower :in lowers
                             :for upper :in uppers
                             :append `((,lower ,lower-val)
                                       (,upper ,upper-val)))))
    (labels ((rec (vars lowers uppers)
               (if (null vars)
                   `(progn ,@body)
                   `(loop :for ,(first vars) :from ,(first lowers) :to ,(first uppers) :do
                          ,(rec (rest vars) (rest lowers) (rest uppers))))))
      `(let ,let-bindings
         ,(rec vars lowers uppers)))))
