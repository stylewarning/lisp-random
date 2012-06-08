(defun id (n)
  (coerce (loop :for i :below n
                :collect (1+ i)) 'vector))

(defun abs> (x y)
  (> (abs x)
     (abs y)))

(defun sign (x)
  (if (plusp x) 1 -1))

(defun leftp (x)
  (plusp x))

(defun rightp (x)
  (not (leftp x)))

(defun mobilep (idx perm &optional (len (length perm)))
  (let ((val (aref perm idx)))
    (cond
      ((leftp val) (and (plusp idx)
                        (abs> val (aref perm (1- idx)))))
      ((rightp val) (and (not (= (1- len) idx))
                         (abs> val (aref perm (1+ idx))))))))

(defun reverse-direction (idx perm)
  (setf (aref perm idx) (- (aref perm idx))))

(defun exists-mobile-p (perm &optional (len (length perm)))
  (loop :for i :below len
        :thereis (mobilep i perm len)))



(defun next-perm (perm &optional (len (length perm)))
  (let ((idx -1)
        (max-mob -1))
    (when (exists-mobile-p perm len)
      ;; Find the largest mobile
      (loop :for i :below len
            :for x :across perm
            :if (and (mobilep i perm len)
                     (abs> x max-mob))
              :do (setf idx     i
                        max-mob x)
            :finally (let ((adj-idx (- idx (sign max-mob))))
                       (when (and (not (minusp adj-idx))
                                  (not (minusp idx)))
                         (rotatef (aref perm idx)
                                  (aref perm adj-idx)))
                       (loop :for i :from 0
                             :for x :across perm
                             :when (abs> x max-mob)
                               :do (reverse-direction i perm))))
      perm)))

(defun perms (n)
  (let ((initial (id n)))
    (loop :for perm := initial :then (next-perm perm)
          :while perm
          :do (print perm))))