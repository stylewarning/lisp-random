;;;; genetic-minimum.lisp
;;;; Copyright (c) 2012 Robert Smith

(ql:quickload :ieee-floats)

(declaim (optimize speed (safety 0) (debug 0)))

(defparameter *population-size* 1024
  "The size of a population.")

(defparameter *elite-rate* 1/10
  "The percentage of the top members that continue living.")

(defparameter *mutation-rate* 1/2
  "The rate at which offspring get mutated.")

(defparameter *maximum-number-of-iterations* 16500
  "The default maximum number of iterations before aborting the simulation.")

(defparameter *target* (lambda (x params)
                         (destructuring-bind (a b c) params
                           ;; a x^2 + b x + c
                           (+ c (* x (+ b (* x a))))))
  "The target function to be optimized.")

(defun random-unit (&optional (n 100))
  (/ (- (random (1+ (* 2 n))) n) n))

(defun generate-target-points (function &optional (start -10)
                                                  (end 10)
                                                  (step 1) 
                                                  (twiddle 0))
  (loop :for i :from start :to end :by step
        :collect (cons i (float (+ (funcall function i)
                                   (* twiddle (random-unit)))))))

(defparameter *target-pts* (generate-target-points (lambda (x) (- (* 3 x x) 4))
                                                   -10
                                                   10
                                                   0.1))

(defstruct (citizen (:conc-name citizen.))
  (chromosome nil)
  (fitness 0 :type integer))

(defconstant +max-float-int+ (1+ (ieee-floats:encode-float32 most-negative-single-float)))

(defconstant nan (+ (* most-negative-single-float 2)
                    (* most-positive-single-float 2)))

(defun random-citizen ()
  "Create a random citizen."
  (make-citizen :chromosome (list (random +max-float-int+)
                                  (random +max-float-int+)
                                  (random +max-float-int+))
                :fitness 0))

(defun citizen< (a b)
  "Check if citizen A is less fit than citizen B. Requires that the
fitness of A and B were updated."
  (< (citizen.fitness a)
     (citizen.fitness b)))

(defun calculate-citizen-fitness (citizen)
  "Calculate the fitness of a citizen CITIZEN."
  (labels ((square (x y)
             (expt (- y (funcall *target*
                                 x
                                 (mapcar 'ieee-floats:decode-float32 
                                         (citizen.chromosome citizen))))
                   2)))
    (loop :for (x . y) :in *target-pts*
          :sum (square x y))))

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

(defun flip-random-bit (integer &optional (number-of-bits 1))
  "Flip a total of NUMBER-OF-BITS in INTEGER randomly. The bits
flipped might not necessarily be distinct."
  (if (not (plusp number-of-bits))
      integer
      (flip-random-bit (logxor integer (ash 1 (random (integer-length integer))))
                       (1- number-of-bits))))

;;; Change me
(defun mutate-citizen (citizen)
  "Cause a random mutation in the chromosome of CITIZEN."
  (map-into (citizen.chromosome citizen)
            (lambda (x) (flip-random-bit x 8))
            (citizen.chromosome citizen))
  citizen)

(defun ones (n &optional (shift 0))
  (ash (1- (ash 1 (1+ n))) shift))

(defun mate-integers (i-mum i-dad &optional percent)
  (let* ((mum-len (integer-length i-mum))
         (dad-len (integer-length i-dad))
         (max-len (max mum-len dad-len)))
    (unless percent
      (setf percent (/ (random max-len) max-len)))
    
    (let* ((mum-bit-length (floor (* max-len percent)))
           (dad-bit-length (- max-len mum-bit-length))
           (mum-bits (ones mum-bit-length dad-bit-length))
           (dad-bits (ones dad-bit-length)))
      (let ((offspring (logior (logand i-mum mum-bits)
                               (logand i-dad dad-bits))))
        (if (<= offspring +max-float-int+)
            offspring
            +max-float-int+)))))

(defun mate-citizens (mum dad &optional percent)
  "Mate citizens MUM and DAD to produce a new citizen. Copy PERCENT
percent of the MUM chromosome to the child. PERCENT is randomized by
default."
  (let ((mums-genes (citizen.chromosome mum))
        (dads-genes (citizen.chromosome dad)))
    (make-citizen :chromosome (mapcar (lambda (x y) (mate-integers x y percent))
                                      mums-genes 
                                      dads-genes))))

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
    (dotimes (i iterations (aref population 0))
      (update-population-fitness population)

      (sort-population population)

      (let ((leader (aref population 0)))
        (format t "Generation ~D: ~S ==> ~A~%"
                i
                (mapcar 'ieee-floats:decode-float32 (citizen.chromosome leader))
                (citizen.fitness leader))
        
        (when (zerop (citizen.fitness leader))
          (return leader)))
      
      (mate population next-generation)
      
      (rotatef population next-generation))))
