(in-package :cl-user)
(defpackage pathwayz.sigmoid
  (:use :cl)
  (:import-from :pathwayz.config
                :config)
  (:import-from :cl-store
                :store
                :restore)
  (:import-from :alexandria
                :iota
                :map-iota
                :clamp
                )
  (:export :nnode
           :make-network
           :make-network
           :set-input
           :propagate
           :+eta+
           :back-propagate
           :buzz
           :save-net
           :load-net
           :sigmoid
           :sigmoid-d
           :leaky-relu
           :leaky-relu-d
           :relu
           :relu-d
           :tanh-d
           ))
(in-package :pathwayz.sigmoid)


(defclass nnode ()
  ((parents :accessor nnparents :initform nil
            :documentation "List of parent nodes")
   (parent-count :accessor nnparent-count :initform 0)
   (children :accessor nnchildren :initform nil
             :documentation "List of child nodes")
   (children-count :accessor nnchild-count :initform 0)
   (weights :accessor nnweights :initform nil
            :documentation "List of input weights")
   (bias :accessor nnbias :initarg :bias :initform 0.0d0
         :documentation "Bias value for node")
   (output :accessor nnoutput :initform 00.0
           :documentation "Last recorded output from this node")
   (delta :accessor nndelta :initform 0.0
          :documentation "Last recorded delta value for this node")
   (transfer :accessor nntransfer :initarg :trans
             :documentation "Transfer function used for this node")
   (deriv :accessor nnderiv :initarg :deriv
          :documentation "Derivative of transfer function, used during back propagation"))
  (:documentation "Defines a single node in a neural network, in full detail."))


(defun coerce-function (obj)
  "Force an object to be a function if it is a symbol."
  (if (symbolp obj)
    (symbol-function obj)
    obj))

(defmethod nn-connect ((parent nnode) (child nnode) &optional (max-random-weight 1.0d0))
  "Connect two nodes in a feed-forward fashion"
  (push child (nnchildren parent))
  (push parent (nnparents child))
  (setf (nnweights child)
        (if (nnweights child)
          (adjust-array (nnweights child) (list (length (nnparents child))))
          (make-array (list (length (nnparents child))) :adjustable t :initial-element 0.0d0)))
  (setf (aref (nnweights child) (1- (length (nnparents child))))
        (random max-random-weight))
  (incf (nnparent-count child))
  (incf (nnchild-count parent)))

(defun make-network (layers &key (max-random-weight 1.0d0) (max-random-bias 1.0d0) 
                                 (default-transfer-function 'sigmoid) 
                                 (default-derivative-function 'sigmoid-d))
  "Make a randomized network. Each layer may be either an integer to 
   use default transfer function, or a list of (size transfer-func derivative-func)"
  (let* ((tfunc (coerce-function default-transfer-function))
         (dfunc (coerce-function default-derivative-function)))
    (labels ((make-empty-node (&optional tf df)
               (make-instance 'nnode 
                              :bias (random max-random-bias)
                              :trans (or tf tfunc)
                              :deriv (or df dfunc)))
             (make-layer (layer-defn)
               (if (listp layer-defn)
                 (let ((n (first layer-defn))
                       (tf (coerce-function (second layer-defn)))
                       (df (coerce-function (third layer-defn))))
                   (map 'list
                        (lambda (n)
                               (declare (ignore n))
                               (make-empty-node tf df))
                        (iota n)))
                 (map 'list 
                      (lambda (n)
                        (declare (ignore n))
                        (make-empty-node))
                      (iota layer-defn)))))
      (let ((net (map 'list #'make-layer layers)))
        ; Fully connect layers
        (do ((ln net (rest ln)))
          ((null ln))
          (dolist (la1 (first ln))
            (dolist (la2 (second ln))
              (nn-connect la1 la2 max-random-weight))))
        ; Return network
        net))))


(defmethod set-input ((node nnode) input-value)
  (with-slots (transfer output) node
    (setf output (funcall transfer input-value))))

(defmethod prop-node ((node nnode))
  (with-slots (parents weights bias output transfer) node
    (setf output
          (funcall transfer 
                   (+ bias
                      (reduce #'+ 
                              (map 'list 
                                   (lambda (p w) 
                                     (* (nnoutput p) w))
                                   parents weights)))))))

(defun propagate (net inputs)
  (map 'list #'set-input (first net) inputs) 
  (dolist (layer (rest net))
    (dolist (node layer)
      (prop-node node)))
  (map 'list #'nnoutput (first (last net))))


(defparameter +eta+ 0.1d0)

(defmethod update-output-delta ((node nnode) expected-output)
  (with-slots (delta deriv output) node
    (setf delta (* (- output expected-output)
                   (funcall deriv output)))))

(defmethod update-hidden-delta ((node nnode) parent-weight-index)
  "Update delta value for hidden layer. Note that parent-weight-index is an optimization
   to avoid needing to search for the parent on each call, since it's a tight loop."
  (with-slots (delta children output deriv) node
    (setf delta
          (* (funcall deriv output)
             (reduce #'+ (map 'list 
                              (lambda (cnode) 
                                (with-slots (weights delta) cnode
                                  (* delta (aref weights parent-weight-index))))
                              children))))))


(defun buzz (net &optional (freq 1000))
  "Randomly set a few weights to random ~~ 0"
  (format t "BUZZ~%")
  (dolist  (layer net)
    (dolist (node layer)
      (map-into (nnweights node)
                (lambda (w)
                  (if (= (random freq) 1)
                    0.0d0 w))
                (nnweights node)))))

(defun back-propagate (net expected-outputs &optional (inputs nil) (eta nil))
  (when inputs (propagate net inputs)) ; (optionally) forward propagate inputs
  (let ((bnet (reverse net))
        (r-eta (or eta +eta+)))
    ; calculate deltas for output nodes
    (map 'list #'update-output-delta (first bnet) expected-outputs)
    ; calculate hidden layer deltas for the remainder of the list except parents
    (dolist (layer (butlast (rest bnet)))
      (do* ((nodes layer (rest nodes))
            (node (first nodes) (first nodes))
            (index 0 (1+ index)))
        ((null node))
        (update-hidden-delta node index)))
    ; update weights and biases for each node
    (dolist (layer bnet)
      (dolist (node layer)
        (with-slots (weights bias delta parents) node
          (map-into weights
                    (lambda (w p)
                      (clamp (- w (* r-eta delta (nnoutput p)))
                             -100.0d0 100.0d0))
                    weights parents)
          (setf bias (- bias (* r-eta delta))))))))





(defun train-maximizer (&optional (net nil))
  " Test function that returns a sample maximizing neural net, trained for 10,000 iterations. "
  (let ((inet (or net (make-network '(3 10 3)))))
    (dotimes (i 100000 inet)
      (let* ((num-a (random 1.0d0))
             (num-b (random 1.0d0))
             (num-c (random 1.0d0))
             (biggest (max num-a num-b num-c)))
        (back-propagate inet
                        (list (if (= num-a biggest) 1.0d0 0.0d0)
                              (if (= num-b biggest) 1.0d0 0.0d0)
                              (if (= num-c biggest) 1.0d0 0.0d0))
                        (list num-a num-b num-c))))))

(defun save-net (net filename)
  (store net filename))

(defmethod load-net (filename)
  (restore filename))


(defun sigmoid (output)
  (handler-case (/ 1.0d0 (+ 1.0d0 (exp (- output))))
    (floating-point-overflow (ex)
      (declare (ignore ex))
      (if (minusp output)
        0.0d0
        1.0d0))))

(defun sigmoid-d (last-output)
  (* last-output
     (- 1.0d0 last-output)))


(defun leaky-relu (output)
  (if (>= output 0)
    output
    (/ output 100.0d0)))
(defun leaky-relu-d (last-output)
  (if (>= last-output 0)
    1.0d0
    0.1d0))

(defun relu (output)
  (if (>= output 0)
    output
    0.0d0))
(defun relu-d (last-output)
  (if (>= last-output 0) 1.0d0 0.0d0))

(defun tanh-d (last-output)
  (let ((th (tanh last-output)))
    (- 1.0d0 (* th th))))



