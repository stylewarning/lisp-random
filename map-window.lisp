;;;; map-window.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(defmacro map-window ((vars vec &optional ret) &body body)
  (assert (and (alexandria:proper-list-p vars)
               (every #'symbolp vars)))
  (let ((num-vars (length vars)))
    (alexandria:once-only (vec)
      (alexandria:with-gensyms (length i)
        `(let ((,length (length ,vec)))
           (cond
             ((< ,length ,num-vars) nil)
             (t
              (let ((,(first vars) nil)
                    ,@(loop :for n :from 1
                            :for var :in (rest vars)
                            :collect `(,var (aref ,vec ,(1- n)))))
                (loop :for ,i :from ,(1- num-vars) :below ,length
                      :do (progn
                            (shiftf ,@vars (aref ,vec ,i))
                            ,@body)))))
           ,ret)))))
