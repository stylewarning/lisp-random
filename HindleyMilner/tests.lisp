;;;; Some tests for `infer.lisp'.

(defmacro try (x)
  `(multiple-value-bind (derived-type type-expr env)
       (derive-type ',x)
     (format t "~A~% :: ~A~% ;; TyExp: ~A~% ;; Env: ~A~2%"
             ',x
             derived-type
             type-expr
             env)))


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
 ((lambda (I) (I I)) (lambda (x) x)))
;; :: (-> T4 T4)

(try
 (let ((ident (lambda (x) x)))
   (ident ident)))
;; :: (-> T4 T4)


(try
 (letrec ((my-length (lambda (l)
                       (if (null? l)
                           0
                           (+ 1 (my-length (cdr l)))))))
         my-length))
;; :: (-> (LIST T4) NUM)

(try
 (letrec ((my-map (lambda (f l)
                    (if (null? l)
                        'nil
                        (cons (f (car l))
                              (my-map f (cdr l)))))))
         my-map))
;; :: (-> (-> T10 T6) (LIST T10) (LIST T6))

(try
 (lambda (x)
   (+ 4 (call/cc (lambda (exit)
                   (if (null? x)
                       (exit 0)
                       (length x)))))))
;; :: (-> (LIST T12) NUM)


;;;;;;;;;;;;;;;;;;;;;;;;;; Buggy test cases ;;;;;;;;;;;;;;;;;;;;;;;;;;

(try
 (let ((f (lambda (x) (* x x))))
   f))
;; :: (-> NUM NUM)

(try
 (lambda (x)
   (let ((y x))
     (+ 1 y))))
;; :: (-> NUM NUM)
