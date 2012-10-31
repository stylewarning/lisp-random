(defmacro with-cond-once (&body body)
  (let ((flags (gensym "FLAGS-")))
    `(let ((,flags (make-hash-table)))
       (macrolet ((cond-once (&body cond-forms)
                    `(cond
                       ,@(loop :for (pred . results) :in cond-forms
                               :for i :from 0
                               :collect
                               `(,pred
                                 (unless (gethash ,i ,',flags)
                                   (setf (gethash ,i ,',flags) t)
                                   ,@results))))))
         ,@body))))

(defun test-cond-once ()
  (with-cond-once
    (dotimes (i 10)
      (let ((r (random 3)))
        (format t "r=~D: " r)
        
        (cond-once
          ((= r 0) (princ "got 0"))
          ((= r 1) (princ "got 1"))
          ((= r 2) (princ "got 2")))
        
        (terpri)))))


(defmacro with-once (&body body)
  (let ((flags (gensym "FLAGS-")))
    `(let ((,flags (make-hash-table)))
       (macrolet ((once ()
                    (let ((g (gensym "ONCE-")))
                      `(unless (gethash ',g ,',flags)
                         (setf (gethash ',g ,',flags) t)))))
         ,@body))))

(defun test-once ()
  (with-once
    (dotimes (i 10)
      (let ((r (random 3)))
        (format t "r=~D: " r)
        
        (cond-once
          ((and (= r 0) (once)) (princ "got 0"))
          ((and (= r 1) (once)) (princ "got 1"))
          ((and (= r 2) (once)) (princ "got 2")))
        
        (terpri)))))