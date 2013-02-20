(declaim (optimize (speed 0) (debug 3) (safety 3)))
(in-package :cl-openddr)

(defclass os-model ()
  ((vendor :initarg :vendor :accessor vendor :initform nil)
   (model :initarg :model :accessor model :initform nil)
   (build :initarg :build :accessor build :initform nil)
   (description :initarg :description :accessor description :initform nil)
   (confidence :initarg :confidence :accessor confidence :initform nil)
   (version :initarg :version :accessor version :initform nil)
   (version-major :initarg :version-major :accessor version-major :initform nil)
   (version-minor :initarg :version-minor :accessor version-minor :initform nil)
   (version-micro :initarg :version-micro :accessor version-micro :initform nil)
   (version-nano :initarg :version-nano :accessor version-nano :initform nil)))

(defmethod print-object ((obj os-model) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~A ~A" (vendor obj) (model obj))))

(def-os-builder android-mozilla-builder (user-agent)
  (when (or (not (contains-android user-agent)))
    (return-from defbuilder nil))
  (let ((splitted-content (cl-ppcre:split '(:sequence ";") (get-inside-pattern user-agent)))
        (model  (make-instance 'os-model
                               :vendor "Google"
                               :model "Android"
                               :confidence 40
                               )))
    (loop for x in splitted-content
       do
         (progn
           (register-groups-bind (version major minor micro nano desc)
               (".*?Android.?((\\d+)\\.(\\d+)(?:\\.(\\d+))?(?:\\.(\\d+))?(.*))" x)
             (declare (ignore desc))
             (if (> (confidence model) 40)
                 (setf (confidence model) 100)
                 (setf (confidence model) 90))
             (when version
               (setf (version model) version))
             (loop for x in (list major minor micro nano)
                for setter in (list #'(setf version-major) #'(setf version-minor) #'(setf version-micro)
                                    #'(setf version-nano))
                do (when x
                     (funcall setter x model))))
           (register-groups-bind (build)
               (".*Build/(.*)(?:[ \\)])?" x)
             (if (> (confidence model) 40)
                 (setf (confidence model) 100)
                 (setf (confidence model) 45))
             (setf (build model) build))))
    model))

(def-os-builder linux-mozilla-builder (user-agent)
  (when (or (contains-android user-agent)
            (not (scan '(:sequence "Linux") user-agent)))
    (return-from defbuilder nil))
  (let ((model (make-instance 'os-model
                              :vendor "-"
                              :model "Linux"
                              :confidence 60)))
    (register-groups-bind (g1 g2)
        (".*(X11;)?.*?Linux[^;]?([^;]*)?;.*" (get-inside-pattern user-agent))
      (when g1
        (incf (confidence model) 10))
      (when g2
        (incf (confidence model) 10)
        (setf (description model) g2)))
    model))

(def-os-builder macos-mozilla-builder (user-agent)
  (when (or (not (scan '(:sequence "Macintosh") user-agent)))
    (return-from defbuilder nil))
  (let ((model (make-instance 'os-model
                              :vendor "Apple"
                              :model "Mac OS X"
                              :confidence 60)))
    (register-groups-bind (version g1 g2 g3)
        (".*(?:(?:Intel)|(?:PPC)).?Mac OS X.?((\\d+)[_\\.](\\d+)(?:[_\\.](\\d+))?).*" (get-inside-pattern user-agent))
      (setf (confidence model) 80)
      (when version
        (setf (version model) version))
      (loop for x in (list g1 g2 g3)
         for setter in (list #'(setf version-major) #'(setf version-minor) #'(setf version-micro)
                             #'(setf version-nano))
         do (when x
              (funcall setter x model))))
    model))


(def-os-builder ios-mozilla-builder (user-agent)
  (when (or (not (contains-ios-devices user-agent)))
    (return-from defbuilder nil))
  (let ((model (make-instance 'os-model
                              :vendor "Apple"
                              :model "IOS"
                              :confidence 40)))
    (register-groups-bind (version g1 g2 g3)
        (".*(?:iPhone)?.?OS.?((\\d+)_(\\d+)(?:_(\\d+))?).*" (get-inside-pattern user-agent))
      (setf (confidence model) 90)
      (when version
        (setf (version model) version))
      (loop for x in (list g1 g2 g3)
         for setter in (list #'(setf version-major) #'(setf version-minor) #'(setf version-micro)
                             #'(setf version-nano))
         do (when x
              (funcall setter x model))))
    model))


;(linux-mozilla-builder "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.17 (KHTML, like Gecko) Chrome/24.0.1312.56 Safari/537.17")

;(macos-mozilla-builder "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10_6_6; zh-cn) AppleWebKit/533.20.25 (KHTML, like Gecko) Version/5.0.4 Safari/533.20.27")

;(ios-mozilla-builder "Mozilla/5.0 (iPad; CPU OS 6_0 like Mac OS X) AppleWebKit/536.26 (KHTML, like Gecko) Version/6.0 Mobile/10A5355d Safari/8536.25")