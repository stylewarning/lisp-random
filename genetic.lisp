;;;; genetic.lisp
;;;; Copyright (c) 2012 Robert Smith


(defparameter *population-size* 2048)
(defparameter *elite-rate* 1/10)
(defparameter *mutation-rate* 1/4)
(defparameter *maximum-number-of-iterations* 16500)

(defparameter *target* "Kasadkad WOW.")

(defstruct (citizen (:conc-name citizen.))
  (str "")
  (fitness 0))

(defun random-char ()
  (code-char (+ 32 (random 90))))

(defun random-string (length)
  (map-into (make-array length :element-type 'character
                               :initial-element #\nul)
            'random-char))

(defun random-citizen ()
  (make-citizen :str (random-string (length *target*))
                :fitness 0))

(defun citizen< (a b)
  (< (citizen.fitness a)
     (citizen.fitness b)))

(defun calculate-citizen-fitness (citizen)
  (loop :for citizen-char :across (citizen.str citizen)
        :for target-char  :across *target*
        :sum (abs (- (char-code citizen-char)
                     (char-code target-char)))))

(defun update-citizen-fitness (citizen)
  (setf (citizen.fitness citizen) (calculate-citizen-fitness citizen))
  
  ;; Return the citizen
  citizen)

(defun update-population-fitness (population)
  (map-into population 'update-citizen-fitness population))

(defun sort-population (population)
  ;; SORT is destructive
  (sort population 'citizen<))

(defun elite-count ()
  (floor (* *population-size* *elite-rate*)))

(defun elitism (population buffer &optional (esize (elite-count)))
  (setf (subseq buffer 0 esize)
        (subseq population 0 esize)))

(defun mutate (citizen)
  (let ((unlucky-character-pos (random (length (citizen.str citizen)))))
    (setf (aref (citizen.str citizen) unlucky-character-pos)
          (random-char)))
  citizen)

(defun mate-citizens (mom dad &optional percent)
  (let ((len (length (citizen.str mom))))
    (unless percent
      (setf percent (/ (random len) len)))
    
    (let ((moms-genes (subseq (citizen.str mom) 0 (floor (* len percent))))
          (dads-genes (subseq (citizen.str dad) (floor (* len percent)))))
      (make-citizen :str (concatenate 'string moms-genes dads-genes)))))

(defun mate (population buffer)
  ;; Keep the elite
  (elitism population buffer)
  
  (loop :for i :from (elite-count) :below *population-size*
        :do (progn
              (let ((mom (aref population (random (floor *population-size* 2))))
                    (dad (aref population (random (floor *population-size* 2)))))
                
                ;; Mate
                (setf (aref buffer i) (mate-citizens mom dad))
                
                ;; Mutate
                (when (< (random 100) (floor (* 100 *mutation-rate*)))
                  (mutate (aref buffer i)))))))

(defun initialize-populations ()
  (let ((population (make-array *population-size* :element-type 'citizen))
        (buffer (make-array *population-size* :element-type 'citizen)))
    (values (map-into population (lambda (x)
                                   (declare (ignore x))
                                   (random-citizen))
                      population)
            (map-into buffer (lambda (x)
                               (declare (ignore x))
                               (make-citizen))
                      buffer))))

(defun run-simulation ()
  (multiple-value-bind (population buffer) (initialize-populations)
    (loop :for i :below *maximum-number-of-iterations*
          :do (progn
                (update-population-fitness population)

                (sort-population population)

                (let ((leader (aref population 0)))
                  (format t "Generation ~D: ~S ==> ~D~%"
                          i
                          (citizen.str leader)
                          (citizen.fitness leader))
                  
                  (when (zerop (citizen.fitness leader))
                    (return nil)))
                
                (mate population buffer)
                
                (rotatef population buffer)))))