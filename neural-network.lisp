;;;; neural-network.lisp
;;;; Copyright (c) 2012 Robert Smith

(defun step-function (&key (threshold 0))
  (lambda (activation)
    (if (< activation threshold)
        1
        0)))

(defun logistic-function (&key (threshold 0)
                               (response 1)
                               (inhibitory-p nil))
  (lambda (activation)
    (- (/ (1+ (exp (/ (- threshold activation)
                      smoothness))))
       (if inhibitory-p 0 1/2))))

(defstruct neuron
  synapse-count
  synapse-weights
  (response-function (logistic-function) :read-only t))

(defun response (neuron inputs)
  (loop :for w :across (neuron-synapse-weights neuron)
        :for x :across inputs
        :sum (* x w) :into activation
        :finally (funcall (neuron-response-function neuron)
                          activation)))

