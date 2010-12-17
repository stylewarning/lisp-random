;;;; Some tests for `infer.lisp'.

;;;; Note that your output might contain different type variable
;;;; names. All that is important is that the type variables in an
;;;; expression are the same if they are the same here (e.g., if you
;;;; have (-> T163 T163) for EXAMPLE4, then it is correct. However, if
;;;; you have (-> T162 T163), then it is incorrect because they are
;;;; not the same, but I display that they are (-> T10 T10).

(defun try (x) (format t "~A~% :: ~A~%~%" x (derive-type x)))

(defvar example1
  '(cdr '(1 2 3)))

(try example1) ; returns: (LIST NUM)

(defvar example2
  '(lambda (x y) (if (car x) (car y) (length y))))

(try example2) ; returns: (-> (LIST BOOL) (LIST NUM) NUM)

(defvar example3
  '(letrec ((fact (lambda (x) (if (< x 2) 1 (* x (fact (- x 1)))))))
           fact))

(try example3) ; returns: (-> NUM NUM)

(defvar example4
  '(let ((ident (lambda (x) x))) (ident ident)))

(try example4) ; returns: (-> T10 T10)

(defvar example5
  '(letrec ((length (lambda (l) (if (null? l) 0 (+ 1 (length (cdr l)))))))
           length))

(try example5) ; returns: (-> (LIST T174) NUM)

(defvar example6
  '(letrec ((map (lambda (f l)
                   (if (null? l) '() (cons (f (car l)) (map f (cdr l)))))))
           map))

(try example6) ; returns: (-> (-> T194 T199) (LIST T194) (LIST T199))

(defvar example7
  '(lambda (x)
     (+ 4 (call/cc (lambda (exit)
                     (if (null? x) (exit 0) (length x)))))))

(try example7) ; returns: (-> (LIST T207) NUM)
