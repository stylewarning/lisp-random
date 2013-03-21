;;;; genetic.lisp
;;;; Copyright (c) 2012 Robert Smith


(defparameter *population-size* 1024
  "The size of a population.")

(defparameter *elite-rate* 1/10
  "The percentage of the top members that continue living.")

(defparameter *mutation-rate* 1/4
  "The rate at which offspring get mutated.")

(defparameter *maximum-number-of-iterations* 16500
  "The default maximum number of iterations before aborting the simulation.")

(defparameter *target* "The target string goes HERE!"
  "The target string to be generated.")

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
  "Check if citizen A is less fit than citizen B."
  (< (citizen.fitness a)
     (citizen.fitness b)))

(defun calculate-citizen-fitness (citizen)
  "Calculate the fitness of a citizen CITIZEN."
  (loop :for citizen-char :across (citizen.chromosome citizen)
        :for target-char  :across *target*
        :sum (abs (- (char-code citizen-char)
                     (char-code target-char)))))

(defun update-citizen-fitness (citizen)
  "Update the fitness of CITIZEN."
  (setf (citizen.fitness citizen) (calculate-citizen-fitness citizen))
  
  ;; Return the citizen
  citizen)

(defun update-population-fitness (population)
  "Calculate the fitness of all citizens in POPULATION."
  (map-into population 'update-citizen-fitness population))

(defun sort-population (population)
  "Sort POPULATION based off of fitness destructively."
  (sort population 'citizen<))

(defun elite-count ()
  "Count the number of elite citizens to keep."
  (floor (* *population-size* *elite-rate*)))

(defun pass-elite-to-next-generation (population next-generation &optional (size (elite-count)))
  "Pass of SIZE elite from POPULATION to NEXT-GENERATION."
  (setf (subseq next-generation 0 size)
        (subseq population 0 size)))

(defun mutate-citizen (citizen)
  "Cause a random mutation in the chromosome of CITIZEN."
  (let ((unlucky-character-pos (random (length (citizen.chromosome citizen)))))
    (setf (aref (citizen.chromosome citizen) unlucky-character-pos)
          (random-char)))
  citizen)

(defun mate-citizens (mum dad &optional percent)
  "Mate citizens MUM and DAD to produce a new citizen. Copy PERCENT
percent of the MUM chromosome to the child. PERCENT is randomized by
default."
  (let ((len (length (citizen.chromosome mum))))
    (unless percent
      (setf percent (/ (random len) len)))
    
    (let ((mums-genes (subseq (citizen.chromosome mum) 0 (floor (* len percent))))
          (dads-genes (subseq (citizen.chromosome dad) (floor (* len percent)))))
      (make-citizen :chromosome (concatenate 'string mums-genes dads-genes)))))

(defun mate (population next-generation)
  "Produce the next generation of citizens. Perform mating in
POPULATION to produce NEXT-GENERATION."
  ;; Keep the elite
  (pass-elite-to-next-generation population next-generation)
  
  (loop :for i :from (elite-count) :below *population-size*
        :do (progn
              (let ((mum (aref population (random (floor *population-size* 2))))
                    (dad (aref population (random (floor *population-size* 2)))))
                
                ;; Mate
                (setf (aref next-generation i) (mate-citizens mum dad))
                
                ;; Mutate
                (when (< (random 100) (floor (* 100 *mutation-rate*)))
                  (mutate-citizen (aref next-generation i)))))))

(defun initialize-populations ()
  "Create new generations. Returns a random population and a buffer
generation."
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
  "Run the genetic algorithm, with a maximum of ITERATIONS iterations."
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
