;;;; Some tests for `infer.lisp'.

(defmacro try (expected &body body)
  (let ((x (first body)))
    `(multiple-value-bind (derived-type type-expr env)
         (derive-type ',x)
       (format t "[~:[FAIL~;PASS~]] ~A~%   :: ~A~% Exp: ~A~% ;; TyExp: ~A~% ;; Env: ~A~2%"
               (equalp ',expected derived-type)
               ',x
               derived-type
               ',expected
               type-expr
               env))))


(try (LIST NUM)
 (cdr '(1 2 3)))


(try (-> (LIST BOOL) (LIST NUM) NUM)
 (lambda (x y)
   (if (car x)
       (car y)
       (length y))))


(try (-> NUM NUM)
 (letrec ((fact (lambda (x)
                  (if (< x 2)
                      1
                      (* x (fact (- x 1)))))))
         fact))


(try (-> T3 T3)
  ((lambda (I) (I I)) (lambda (x) x)))


(try (-> T3 T3)
 (let ((ident (lambda (x) x)))
   (ident ident)))


(try (-> (LIST T4) NUM)
 (letrec ((my-length (lambda (l)
                       (if (null? l)
                           0
                           (+ 1 (my-length (cdr l)))))))
         my-length))


(try (-> (-> T10 T6) (LIST T10) (LIST T6))
 (letrec ((my-map (lambda (f l)
                    (if (null? l)
                        'nil
                        (cons (f (car l))
                              (my-map f (cdr l)))))))
         my-map))


(try (-> (LIST T6) NUM)
 (lambda (x)
   (+ 4 (call/cc (lambda (exit)
                   (if (null? x)
                       (exit 0)
                       (length x)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;; Buggy test cases ;;;;;;;;;;;;;;;;;;;;;;;;;;

(try (-> NUM NUM)
 (let ((f (lambda (x) (* x x))))
   f))


(try (-> NUM NUM)
 (lambda (x)
   (let ((y x))
     (+ 1 y))))
