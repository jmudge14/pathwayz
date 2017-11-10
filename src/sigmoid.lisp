(in-package :cl-user)
(defpackage pathwayz.sigmoid
  (:use :cl)
  (:import-from :pathwayz.config
                :config)
  (:export :make-network 
           :perceive
           :back-propagate
           :input-size))
(in-package :pathwayz.sigmoid)

(defun sigmoid (z)
  (declare (optimize (safety 0) (speed 3))
           (type float z))
  (handler-case (/ 1 (+ 1 (exp (- z))))
    (floating-point-overflow (ex)
      (declare (ignore ex))
      (if (minusp z)
        0.0
        1.0))))


(defclass neural-network ()
  ((layers :accessor net-layers :initarg :layers)
   (lcount :accessor net-lcount :initarg :lcount)
   (weights :accessor net-weights :initarg :weights)
   (biases :accessor net-biases :initarg :biases)
   (last-outputs :accessor last-outputs :initarg :last-outputs)
   (namblas :accessor net-namblas :initarg :namblas)))

(defun make-network (layers &key (weight 1.0) (bias 1.0))
  (declare (optimize (safety 0) (speed 3)))
  (let* ((rel-layers (make-array (list (length layers))
                                 :initial-contents layers
                                 :element-type 'integer))
         (lcount (length layers))
         (max-layer (apply #'max layers))
         (weights (make-array (list lcount max-layer max-layer) 
                              :element-type 'float
                              :initial-element 0.0))
         (biases (make-array (list lcount max-layer) 
                             :element-type 'float
                             :initial-element 0.0))
         ; For back-propagation, pre-allocated arrays:
         (last-outputs (make-array (list lcount max-layer) 
                                   :element-type 'float
                                   :initial-element 0.0))
         (namblas (make-array (list lcount max-layer) 
                              :element-type 'float
                              :initial-element 0.0)))
    ; randomize weights
    (dotimes (i (array-total-size weights))
      (setf (row-major-aref weights i)
            (random weight)))
    ; randomize biases
    (dotimes (i (array-total-size biases))
      (setf (row-major-aref biases i)
            (random bias)))
    ; return value - a new neural network!
    (make-instance 'neural-network 
                   :layers rel-layers 
                   :lcount lcount 
                   :weights weights 
                   :biases biases 
                   :last-outputs last-outputs
                   :namblas namblas)))

(defmethod input-size ((network neural-network))
  (declare (optimize (safety 0) (speed 3)))
  (aref (net-layers network) 0))

(defmethod layer-size ((network neural-network) layer)
  "Maximum index for a given layer"
  (declare (optimize (safety 0) (speed 3)))
  (1- (aref (net-layers network) layer)))

(defmacro net-bias (network layer cell)
  `(aref (net-biases ,network) ,layer ,cell))

(defmacro net-weight (network layer col index)
  `(aref (net-weights ,network) ,layer ,col ,index))

(defmacro last-output (network layer col)
  `(aref (last-outputs ,network) ,layer ,col))

(defmacro net-nambla (network layer col)
  `(aref (net-namblas ,network) ,layer ,col))

(defmethod perceive ((network neural-network) inputs)
  (declare (optimize (safety 0) (speed 3))
           (type (vector float) inputs))
  (unless (= (length inputs) (input-size network))
    (error "Input length must match network input size."))
  (dotimes (layer (net-lcount network))
    (dotimes (col (1+ (layer-size network layer)))
      (setf (last-output network layer col)
            (if (= layer 0)
              (elt inputs col)
              (sigmoid (+ (net-bias network layer col)
                          (loop for i from 0 upto (layer-size network (1- layer))
                                summing (* (last-output network (1- layer) i)
                                           (net-weight network layer col i)))))))))
  ; return outputs - efficiently
  (loop with o = (last-outputs network)
        with l = (1- (net-lcount network))
        for i from 0 upto (layer-size network l)
        collect (aref o l i)))


(defun back-propagate (network inputs desired-outputs &key (eta 0.1))
  "Back-propagate to update the given network toward the given desired-outputs."
  (declare (optimize (safety 0) (speed 3))
           (type (vector float *) inputs desired-outputs)
           (type float eta))
  (let* ((last-layer (1- (net-lcount network))))
    ; Phase 1 - Forward pass
    (perceive network inputs)

    ; Phase 2 - Calculate nambla values for output layer
    (loop for layer from last-layer downto 1 do
          (loop for col from 0 upto  (layer-size network layer)do
                (let* ((output (last-output network layer col)))
                  (setf (net-nambla network layer col)
                        (* output
                           (- 1 output)
                           (if (= layer last-layer)
                             (- output (aref desired-outputs col))
                             (loop for i from 0 upto (layer-size network (1+ layer))
                                   summing (* (net-nambla network (1+ layer) i)
                                              (net-weight network (1+ layer) col i)))))))))
    ; Phase 3 - Update
    (loop for layer from last-layer downto 1 do ; ignore input layer
          (loop for col from 0 upto  (layer-size network layer)
                for nambla = (net-nambla network layer col) do
                ; update biases
                (setf (net-bias network layer col)
                      (+ (net-bias network layer col)
                         (* (- eta) nambla)))
                ; update weights
                (loop for ind from 0 upto  (layer-size network (1- layer))do
                      (setf (net-weight network layer col ind)
                            (+ (net-weight network layer col ind)
                               (* (- eta)
                                  nambla
                                  (last-output network (1- layer) ind)))))))))


