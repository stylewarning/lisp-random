;;;; Some tests for `infer.lisp'.

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
;; :: (-> T4 T4)

(try 
 (letrec ((length (lambda (l)
                    (if (null? l)
                        0
                        (+ 1 (length (cdr l)))))))
         length))
;; :: (-> (LIST T4) NUM)

(try 
 (letrec ((map (lambda (f l)
                 (if (null? l)
                     'nil
                     (cons (f (car l))
                           (map f (cdr l)))))))
         map))
;; :: (-> (-> T10 T6) (LIST T10) (LIST T6))

(try 
 (lambda (x)
   (+ 4 (call/cc (lambda (exit)
                   (if (null? x)
                       (exit 0)
                       (length x)))))))
;; :: (-> (LIST T12) NUM)

(try
 (let ((f (lambda (x) (* x x))))
   f))
;; :: (-> NUM NUM)
