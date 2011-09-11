;;;; Some tests for `infer.lisp'.

;;;; Note that your output might contain different type variable
;;;; names. All that is important is that the type variables in an
;;;; expression are the same if they are the same here (e.g., if you
;;;; have (-> T163 T163) for EXAMPLE4, then it is correct. However, if
;;;; you have (-> T162 T163), then it is incorrect because they are
;;;; not the same, but I display that they are (-> T10 T10).

(defmacro try (x)
  `(format t "~A~% :: ~A~%~%" ',x (derive-type ',x)))


(try
 (cdr '(1 2 3)))
;; :: (LIST NUM)

(try 
 (lambda (x y)
   (if (car x)
       (car y)
       (length y))))
;; :: (-> (LIST BOOL) (LIST NUM) NUM)

(try 
 (letrec ((fact (lambda (x)
                  (if (< x 2)
                      1
                      (* x (fact (- x 1)))))))
         fact))
;; :: (-> NUM NUM)

(try 
 (let ((ident (lambda (x) x)))
   (ident ident)))
;; :: (-> T10 T10)

(try 
 (letrec ((length (lambda (l)
                    (if (null? l)
                        0
                        (+ 1 (length (cdr l)))))))
         length))
;; :: (-> (LIST T174) NUM)

(try 
 (letrec ((map (lambda (f l)
                 (if (null? l)
                     'nil
                     (cons (f (car l))
                           (map f (cdr l)))))))
         map))
;; :: (-> (-> T194 T199) (LIST T194) (LIST T199))

(try 
 (lambda (x)
   (+ 4 (call/cc (lambda (exit)
                   (if (null? x)
                       (exit 0)
                       (length x)))))))
;; :: (-> (LIST T207) NUM)
