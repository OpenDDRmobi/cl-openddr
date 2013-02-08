(in-package :cl-openddr)

(defclass identified ()
  ((vendor :initarg vendor :accessor vendor)
   (model :initarg model :accessor model)
   (version-major :initform nil :initarg :version-major :accessor version-major)
   (version-minor :initform nil :initarg :version-minor :accessor version-minor)
   (version-micro :initform nil :initarg :version-micro :accessor version-micro)
   (version-nano :initform nil :initarg :version-nano :accessor version-nano)
   (reference-browser :initform nil :initarg :reference-browser :accessor reference-browser)
   (reference-browser-version :initform nil :initarg :reference-browser-version :accessor reference-browser-version)
   (display-width :initform nil :initarg :display-width :accessor display-width)
   (display-height :initform nil :initarg :display-height :accessor display-height)
   (confidence :initform nil :initarg :confidence :accessor confidence)
   (layout-engine :initform nil :initarg :layout-engine :accessor layout-engine)
   (layout-engine-version :initform nil :initarg :layout-engine-version :accessor layout-engine-version)
   ))


(defmacro def-layout-engine-builder (name type params &body body)
  `(defun ,name ,params 
     (block defbuilder
       ,@body)))
