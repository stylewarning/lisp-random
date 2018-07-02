;;;; distributed-median.lisp
;;;;
;;;; Copyright (c) 2018 Robert Smith

;;;; See median.lisp for the problem statement.
;;;;
;;;; This file implements a distributed version. We simulate a
;;;; distributed system using threads and message passing. We only
;;;; assume each thread has some local storage, plus the ability to
;;;; send data to other threads. (We can send objects, but we don't
;;;; rely on the objects being shared in memory.)


;;; To use this file, LOAD it, and call
;;;
;;;     (LOAD-AND-COMPUTE (GENERATE-VECTORS 10 5000000))
;;;
;;; This will simulate a distributed system with 10 machines, each
;;; with 5 million numbers.

(require :sb-concurrency)

;;;;;;;;;;;;;;;;;;;;;;;;;;; Machine Setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (machine (:constructor %make-machine))
  "A representation of an independent machine."
  ;; The machine's ID. The machine ID=0 is the master machine, and
  ;; doesn't have any data.
  id
  ;; The vector of data (fixnums).
  data
  ;; The mailbox to receive messages.
  mailbox
  ;; Thread of execution owned by this machine.
  thread)

(defun make-machine (id)
  "Make a machine with the non-negative id ID."
  (check-type id (and fixnum unsigned-byte))
  (%make-machine :id id
                 :mailbox (sb-concurrency:make-mailbox
                           :name (format nil "Machine ~D" id))))

(sb-ext:defglobal **machines** nil
  "The collection of machines in the distributed system.")

(defun master-mailbox ()
  "Mailbox of the master."
  (machine-mailbox (aref **machines** 0)))

(defun worker-mailbox (id)
  "Mailbox of the worker whose id is ID."
  (assert (plusp id))
  (machine-mailbox (aref **machines** id)))

(defun num-workers ()
  "The number of workers."
  (1- (length **machines**)))

(defun initialize-machines (m)
  "Initialize M+1 machines, one of which is the master and M of which
are workers."
  (format t "Initializing machines...~%")
  (let ((machines (make-array (1+ m))))
    (dotimes (i (1+ m))
      (setf (aref machines i) (make-machine i)))
    (setf **machines** machines)))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Communication ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Our message structure is
;;;
;;;     (:TAG &optional payload)
;;;
;;; The workers will respond to :DIST, :GLB, :LUB, and :HALT messages.

(defun send-to-workers (message)
  "Send the message MESSAGE to all of the workers."
  (loop :for i :from 1 :below (length **machines**) :do
    (sb-concurrency:send-message (machine-mailbox (aref **machines** i)) message)))

(defun receive-responses ()
  "Receive a response back from each worker."
  (loop :repeat (num-workers)
        :collect (sb-concurrency:receive-message (master-mailbox))))


;;;;;;;;;;;;;; Distribution Computation & Manipulation ;;;;;;;;;;;;;;;

;;; In the functions definitions below, we add a label comment to
;;; indicate who we expect to call this function.

(defstruct dist
  "A simple tuple of numbers representing the relative distribution of
a number."
  (less-than    0 :type unsigned-byte)
  (equal-to     0 :type unsigned-byte)
  (greater-than 0 :type unsigned-byte))

(defun compute-single-dist (n vector)   ; Worker
  "Compute the distribution information about a number N of a vector
VECTOR. The distribution information tells us how many numbers in the
machine are less than, equal to, or greater than the number N."
  (let ((<n 0) (=n 0) (>n 0))
    (map nil
         (lambda (x)
           (cond
             ((< x n) (incf <n))
             ((> x n) (incf >n))
             (t       (incf =n))))
         vector)
    
    ;; Construct and return the distribution.
    (make-dist :less-than    <n
               :equal-to     =n
               :greater-than >n)))

(defun add-dists (d1 d2)                ; Master
  "Add two distributions D1 and D2."
  (make-dist
   :less-than    (+ (dist-less-than d1) (dist-less-than d2))
   :equal-to     (+ (dist-equal-to d1) (dist-equal-to d2))
   :greater-than (+ (dist-greater-than d1) (dist-greater-than d2))))

(defun compute-dist (n)                 ; Master
  "Compute the distribution information of the cluster for the number N."
  ;; Tell everybody to compute their distribution.
  (send-to-workers `(:DIST ,n))

  ;; Compute the total distribution.
  (loop :with d := (make-dist :less-than 0 :equal-to 0 :greater-than 0)
        :for response :in (receive-responses)
        :do (assert (eq ':DIST (first response)))
            (setf d (add-dists d (second response)))
        :finally (return (values (dist-less-than    d)
                                 (dist-equal-to     d)
                                 (dist-greater-than d)))))


;;;;;;;;;;;;;;;;;;;;;;;;; Bound Computations ;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-lub (n data)                ; Worker
  "Find the smallest number greater than N in the data set DATA."
  (loop :with lub = nil
        :for x :across data
        :do (when (and (or (null lub) (< x lub))
                       (> x n))
              (setf lub x))
        :finally (return lub)))

(defun find-glb (n data)                ; Worker
  "Find the largest number less than N in the data set DATA."
  (loop :with glb = nil
        :for x :across data
        :do (when (and (or (null glb) (> x glb))
                       (< x n))
              (setf glb x))
        :finally (return glb)))

(defun find-global-lub (n)              ; Master
  "Find the smallest number greater than N in all of the data across
all of the machines."
  ;; Tell everybody to compute their LUB.
  (send-to-workers `(:LUB ,n))

  ;; Compute the total LUB.
  (loop :with lub := most-positive-fixnum
        :for response :in (receive-responses)
        :do (assert (eq ':LUB (first response)))
            (setf lub (min lub (second response)))
        :finally (return lub)))

(defun find-global-glb (n)              ; Master
  "Find the largest number less than N in the all of the data across
all of the machines."
  ;; Tell everybody to compute their LUB.
  (send-to-workers `(:GLB ,n))

  ;; Compute the total GLB.
  (loop :with glb := most-negative-fixnum
        :for response :in (receive-responses)
        :do (assert (eq ':GLB (first response)))
            (setf glb (max glb (second response)))
        :finally (return glb)))

;;;;;;;;;;;;;;;;;;;;;;;; Median Computations ;;;;;;;;;;;;;;;;;;;;;;;;;

(defun naive-median (vector)
  "Naively compute the median of the vector VECTOR."
  (setf vector (sort (copy-seq vector) #'<))
  (let* ((n (length vector))
         (m (floor n 2)))
    (if (oddp n)
        (aref vector m)
        (/ (+ (aref vector (1- m))
              (aref vector m))
           2))))

(defun half (x)
  (round x 2))

(defun compute-median (&key (min most-negative-fixnum)
                            (max most-positive-fixnum)) ; Master
  "Compute the median across all machines, where the minimum number on
any machine is expressed by MIN and the maximum by MAX."
  (labels ((bisect (n min max)
             (format t "Trying N=~A in (~A, ~A)~%" n min max)
             ;; Have we closed our interval to [X, X+1]?
             (if (= (1+ min) max)
                 (/ (+ min max) 2)    ; Found median.
                 (multiple-value-bind (<n =n >n) (compute-dist n)
                   (if (zerop =n)
                       ;; We did not find N in the data set, so we
                       ;; either need to balance out <N and >N by
                       ;; bisecting, or we are dead in the middle, and
                       ;; we need to find the numbers closest to N.
                       (cond
                         ((< <n >n) (bisect (half (+ n max)) n max))
                         ((> <n >n) (bisect (half (+ n min)) min n))
                         (t            
                          ;; This case sort of sucks. We are in the
                          ;; middle of the data set with our number N
                          ;; (which isn't actually a part of the
                          ;; data), and we need to find the two
                          ;; numbers P and Q such that P is the
                          ;; greatest number < N, and Q is the
                          ;; smallest number > N.
                          (let ((lub (find-global-lub n))
                                (glb (find-global-glb n)))
                            (/ (+ lub glb) 2)))) ; Found median.
                       
                       ;; We did find N in the data set. Suppose
                       ;; 
                       ;;     A = count of numbers < N,
                       ;;     B = count of numbers > N, and
                       ;;     C = count of numbers = N,
                       ;;     
                       ;; then we want A and B balanced up to C. In
                       ;; other words, we want
                       ;;
                       ;;     | A - B | < C.
                       ;;
                       ;; If they are balanced up to C, then N is the
                       ;; median. Otherwise, we rebalance by bisecting
                       ;; (which is done by checking if A - B is
                       ;; positive or negative, i.e., if A > B, vice
                       ;; versa.
                       (let ((delta (- <n >n)))
                         (cond
                           ((< (abs delta) =n) n)  ; Found median.
                           ((plusp delta) (bisect (half (+ n min)) min n))
                           (t             (bisect (half (+ n max)) n max)))))))))
    (bisect (half (+ max min)) min max)))


;;;;;;;;; Orchestration of the Distributed System Simulation ;;;;;;;;;

(defun load-vectors-onto-machines (vectors)
  "Given M vectors VECTORS, load them onto the M worker machines."
  (format t "Loading vectors...~%")
  (assert (= (length vectors) (num-workers)))
  (loop :for i :from 1 :below (length **machines**) :do
    (setf (machine-data (aref **machines** i)) (elt vectors (1- i)))))

;;; Entry point to the master machine.
(defun %master-entry ()
  ;; Compute the median.
  (setf (machine-data (aref **machines** 0)) (compute-median))
  ;; Send a HALT message to everybody.
  (send-to-workers '(:HALT)))

;;; Function to generate the entry point to the worker machine with id
;;; ID.
(defun %worker-entry-function (id)
  (assert (plusp id))
  (lambda ()
    (let ((mailbox (machine-mailbox (aref **machines** id)))
          (data    (machine-data    (aref **machines** id))))
      ;; Keep looping until we are told to stop.
      (loop
        (let ((r (sb-concurrency:receive-message mailbox)))
          (ecase (first r)
            (:DIST (let* ((n (second r))
                          (d (compute-single-dist n data)))
                     (sb-concurrency:send-message (master-mailbox) `(:DIST ,d))))
            (:LUB (let* ((n (second r))
                         (lub (find-lub n data)))
                    (sb-concurrency:send-message (master-mailbox) `(:LUB ,lub))))
            (:GLB (let* ((n (second r))
                         (glb (find-glb n data)))
                    (sb-concurrency:send-message (master-mailbox) `(:GLB ,glb))))
            (:HALT (return))))))))


(defun compute-distributed-median ()
  "Fire off all of the threads (i.e., start each machine at its entry
point) to compute the distributed mean. The result will be deposited
into the DATA slot of the master machine."
  ;; Start all of the workers.
  (loop :for i :from 1 :below (length **machines**) :do
    (setf (machine-thread (aref **machines** i))
          (sb-thread:make-thread (%worker-entry-function i) :name (format nil "Worker ~D" i))))
  ;; Start the master.
  (setf (machine-thread (aref **machines** 0))
        (sb-thread:make-thread #'%master-entry :name "Master"))
  ;; Wait for it all to finish. (We could just let the master finish,
  ;; and it'd work fine.)
  (loop :for m :across **machines** :do
    (sb-thread:join-thread (machine-thread m)))
  ;; Read the answer.
  (machine-data (aref **machines** 0)))


(defun load-and-compute (vectors)
  "Load the vectors VECTORS onto the machine, and compute their median. Return the list:

     (:COMPUTED <computed median>
      :NAIVE    <naively computed median>
      :EQUAL    <whether they're equal>)"
  ;; These two steps would already be "done" in a distributed
  ;; system. We are just simulating one, and these are artifacts of
  ;; that.
  (initialize-machines (length vectors))
  (load-vectors-onto-machines vectors)
  ;; Do the computation.
  (let ((computed (time (compute-distributed-median)))
        (naive (time (naive-median (apply 'concatenate 'vector vectors)))))
    (list ':computed computed           ; What we computed.
          ':naive naive                 ; The right answer.
          ':equal (= computed naive)))) ; Whether they're equal.

;;;;;;;;;;;;;;;;;;;;; Testing & Data Generation ;;;;;;;;;;;;;;;;;;;;;;

(defun generate-vectors (m n)
  "Generate M vectors, each of length N, +/- a 30% of N."
  (format t "Generating vectors...~%")
  (labels ((random-between (min max)
             (+ min (random (- max min))))
           (random-vector ()
             (let* ((length (random-between (round (* 0.7 n)) (round (* 1.3 n))))
                    (a (make-array length :element-type 'fixnum)))
               (dotimes (i length a)
                 (setf (aref a i) (random-between most-negative-fixnum most-positive-fixnum))))))
    (loop :repeat m :collect (random-vector))))
