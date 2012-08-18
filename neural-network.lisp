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
                      response))))
       (if inhibitory-p 0 1/2))))

(defstruct (neuron (:constructor %make-neuron))
  connectivity
  synapse-weights
  (response-function (logistic-function) :read-only t))

(defun make-neuron (connectivity)
  (%make-neuron :connectivity connectivity
                :synapse-weights (make-list connectivity :initial-element 1)))

(defun response (neuron inputs)
  (loop :for w :in (neuron-synapse-weights neuron)
        :for x :in inputs
        :sum (* x w) :into activation
        :finally (funcall (neuron-response-function neuron)
                          activation)))

(defstruct (neuron-layer (:constructor %make-neuron-layer))
  neurons)

(defun make-neuron-layer (neuron-count connectivity)
  (loop :repeat neuron-count
        :collect (make-neuron connectivity) :into neurons
        :finally (return (%make-neuron-layer :neurons neurons))))

(defun layer-output (layer inputs)
  (loop
    :for neuron :in (neuron-layer-neurons layer)
    :collect (response neuron inputs)))

(defstruct (neural-network (:constructor %make-neural-network))
  input-count
  output-count
  hidden-layer-count
  layers)

(defun fire-network (net inputs)
  (loop :for layer :in (neural-network-layers net)
        :for outputs := (layer-output layer inputs)
          :then (layer-output layer outputs)
        :finally (return outputs)))

(defun make-neural-network (input-count
                            output-count
                            layer-count
                            layer-sizes)
  (assert (plusp layer-count))
  
  ;; For a "Complete Neural Network",
  ;; 
  ;;  (A) The number of synapses per neuron in the first layer equals
  ;;      the number of inputs.
  ;; 
  ;;  (B) The number of neurons in the last layer equals the number of
  ;;      outputs.
  ;; 
  ;;  (C) If the Kth layer has N neurons, then the (K+1)th layer must
  ;;      have N synapses per neuron.
  
  (let ((layers nil))
    (labels ((build-layers (neurons-needed
                            synapses-needed
                            layers-left)
               
               ;; Build the next layer.
               (push (make-neuron-layer neurons-needed
                                        synapses-needed)
                     layers)
               
               ;; Loop.
               (if (null layers-left)
                   (setf layers (nreverse layers))
                   (build-layers (car layers-left)
                                 neurons-needed          ; Rule (C)
                                 (cdr layers-left)))))
      (let ((layer-sizes (append layer-sizes
                                 (list output-count))))  ; Rule (B)
        (if (= 1 layer-count)
            (push (make-neuron-layer input-count output-count)
                  layers)
            (build-layers (car layer-sizes)
                          input-count                    ; Rule (A)
                          (cdr layer-sizes)))) 
      (%make-neural-network :input-count input-count
                            :output-count output-count
                            :hidden-layer-count (max 0 (- layer-count 2))
                            :layers layers))))
