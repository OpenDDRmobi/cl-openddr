(declaim (optimize (speed 0) (debug 3) (safety 3)))
(in-package :cl-openddr)

(defclass identified ()
  ((vendor :initarg :vendor :accessor vendor)
   (model :initarg :model :accessor model)
   (version-major :initform nil :initarg :version-major :accessor version-major)
   (version-minor :initform nil :initarg :version-minor :accessor version-minor)
   (version-micro :initform nil :initarg :version-micro :accessor version-micro)
   (version-nano :initform nil :initarg :version-nano :accessor version-nano)
   (reference-browser :initform nil :initarg :reference-browser :accessor reference-browser)
   (reference-browser-version :initform nil :initarg :reference-browser-version :accessor reference-browser-version)
   (display-width :initform nil :initarg :display-width :accessor display-width)
   (display-height :initform nil :initarg :display-height :accessor display-height)
   (confidence :initform nil :initarg :confidence :accessor confidence)
   (layout-engine :initform nil :initarg :layout-engine :accessor layout-engine-of)
   (layout-engine-version :initform nil :initarg :layout-engine-version :accessor layout-engine-version)
   ))

(defmethod print-object ((obj identified) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~A ~A ~A.~A" (vendor obj) (model obj) (version-major obj) (version-minor obj))))

(defmacro def-layout-engine-builder (name params &body body)
  `(defun ,name ,params 
     (block defbuilder
       ,@body)))

(defmacro def-os-builder (name params &body body)
  `(defun ,name ,params 
     (block defbuilder
       ,@body)))

(defmacro user-agent-contains (regexp stringvar)
  `(cl-ppcre:scan ,regexp ,stringvar))

(defun contains-android (user-agent)
  (user-agent-contains '(:sequence "Android") user-agent))

(defun has-mozilla-pattern (user-agent)
  (register-groups-bind (pre opera-or-mozilla version inside post)
      ("(.*?)((?:Mozilla)|(?:Opera))[/ ](\\d+\\.\\d+).*?\\(((?:.*?)(?:.*?\\(.*?\\))*(?:.*?))\\)(.*)" user-agent)
    (declare (ignore pre inside post))
    (if (cl-ppcre:scan "Opera" opera-or-mozilla)
        nil
        (values t version))))

(defun has-opera-pattern (user-agent)
  (register-groups-bind (pre opera-or-mozilla version inside post)
      ("(.*?)((?:Mozilla)|(?:Opera))[/ ](\\d+\\.\\d+).*?\\(((?:.*?)(?:.*?\\(.*?\\))*(?:.*?))\\)(.*)" user-agent)
    (declare (ignore pre inside))
    (when (cl-ppcre:scan "Opera" opera-or-mozilla)
      (let ((version version))
        (when (string= version "9.80")
          (register-groups-bind (v) (".*Version/(\\d+.\\d+).*" post)
            (setf version v)))
        (values t version)))))

(defun get-inside-pattern (user-agent)
  (register-groups-bind (pre opera-or-mozilla version inside post)
      ("(.*?)((?:Mozilla)|(?:Opera))[/ ](\\d+\\.\\d+).*?\\(((?:.*?)(?:.*?\\(.*?\\))*(?:.*?))\\)(.*)" user-agent)
    (declare (ignore opera-or-mozilla pre version post))
    inside))

(defun contains-ios-devices (user-agent)
  (or (cl-ppcre:scan ".*(?!like).iPad.*" user-agent)
      (cl-ppcre:scan ".*(?!like).iPod.*" user-agent)
      (cl-ppcre:scan ".*(?!like).iPhone.*" user-agent)))

(defun contains-blackberry-or-rim (user-agent)
  (cl-ppcre:scan ".*[Bb]lack.?[Bb]erry.*|.*RIM.?Tablet.?OS.*" user-agent))

(defun contains-symbian (user-agent)
  (cl-ppcre:scan
   ".*Symbian.*|.*SymbOS.*|.*Series.?60.*" user-agent))

(defun contains-windows-phone (user-agent)
  (cl-ppcre:scan ".*Windows.?(?:(?:CE)|(?:Phone)|(?:NT)|(?:Mobile)).*" user-agent))

(defun contains-msie (user-agent)
  (cl-ppcre:scan ".*MSIE.([0-9\\.b]+).*" user-agent))

