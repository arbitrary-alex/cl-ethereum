(in-package #:solidity)

(defgeneric get-property (object property)
  (:documentation ""))

(defgeneric (setf get-property) (value object property)
  (:documentation ""))

(defclass property-mixin ()
  ((properties :initform nil
               :initarg :properties
               :accessor properties)))

(defmethod get-property ((object property-mixin) property)
  (with-slots (properties) object
    (getf properties property)))

(defmethod (setf get-property) (value (object property-mixin) property)
  (with-slots (properties) object
    (setf (getf properties property) value)))

(defclass property-undirected-edge (property-mixin cl-graph:graph-container-undirected-edge)
  ())

(defclass property-directed-edge (property-mixin cl-graph:graph-container-directed-edge)
  ())

(defclass property-vertex (property-mixin cl-graph:graph-container-vertex)
  ((edges-by-property
    :initform (make-hash-table)
    :reader vertex-edges-by-property)))

(defclass property-graph-mixin ()
  ()
  (:default-initargs
   :vertex-class 'property-vertex
   :directed-edge-class 'property-directed-edge
   :undirected-edge-class 'property-undirected-edge))

(defclass property-graph (property-graph-mixin cl-graph:graph-container)
  ())
