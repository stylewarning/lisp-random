;;;; genetic.lisp
;;;; Copyright (c) 2012 Robert Smith


(defparameter *population-size* 2048)
(defparameter *elite-rate* 1/10)
(defparameter *mutation-rate* 1/4)
(defparameter *maximum-number-of-iterations* 16500)

(defparameter *target* "The target string goes HERE!")

(defstruct (citizen (:conc-name citizen.))
  (chromosome "")
  (fitness 0))

(defun random-char ()
  "Generate a random character."
  (code-char (+ 32 (random 90))))

(defun random-string (length)
  "Generate a random string of length LENGTH."
  (map-into (make-array length :element-type 'character
                               :initial-element #\nul)
            'random-char))

(defun random-citizen ()
  "Create a random citizen."
  (make-citizen :chromosome (random-string (length *target*))
                :fitness 0))

(defun citizen< (a b)
  (< (citizen.fitness a)
     (citizen.fitness b)))

(defun calculate-citizen-fitness (citizen)
  (loop :for citizen-char :across (citizen.chromosome citizen)
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

(defun pass-elite-to-next-generation (population next-generation &optional (size (elite-count)))
  (setf (subseq next-generation 0 size)
        (subseq population 0 size)))

(defun mutate-citizen (citizen)
  (let ((unlucky-character-pos (random (length (citizen.chromosome citizen)))))
    (setf (aref (citizen.chromosome citizen) unlucky-character-pos)
          (random-char)))
  citizen)

(defun mate-citizens (mom dad &optional percent)
  (let ((len (length (citizen.chromosome mom))))
    (unless percent
      (setf percent (/ (random len) len)))
    
    (let ((moms-genes (subseq (citizen.chromosome mom) 0 (floor (* len percent))))
          (dads-genes (subseq (citizen.chromosome dad) (floor (* len percent)))))
      (make-citizen :chromosome (concatenate 'string moms-genes dads-genes)))))

(defun mate (population next-generation)
  ;; Keep the elite
  (pass-elite-to-next-generation population next-generation)
  
  (loop :for i :from (elite-count) :below *population-size*
        :do (progn
              (let ((mom (aref population (random (floor *population-size* 2))))
                    (dad (aref population (random (floor *population-size* 2)))))
                
                ;; Mate
                (setf (aref next-generation i) (mate-citizens mom dad))
                
                ;; Mutate
                (when (< (random 100) (floor (* 100 *mutation-rate*)))
                  (mutate-citizen (aref next-generation i)))))))

(defun initialize-populations ()
  (let ((population (make-array *population-size* :element-type 'citizen))
        (next-generation (make-array *population-size* :element-type 'citizen)))
    (values (map-into population (lambda (x)
                                   (declare (ignore x))
                                   (random-citizen))
                      population)
            (map-into next-generation (lambda (x)
                               (declare (ignore x))
                               (make-citizen))
                      next-generation))))

(defun run-simulation (&optional (iterations *maximum-number-of-iterations*))
  (multiple-value-bind (population next-generation) (initialize-populations)
    (dotimes (i iterations)
      (update-population-fitness population)

      (sort-population population)

      (let ((leader (aref population 0)))
        (format t "Generation ~D: ~S ==> ~D~%"
                i
                (citizen.chromosome leader)
                (citizen.fitness leader))
        
        (when (zerop (citizen.fitness leader))
          (return leader)))
      
      (mate population next-generation)
      
      (rotatef population next-generation))))