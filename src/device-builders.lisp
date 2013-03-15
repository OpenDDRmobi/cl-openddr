(declaim (optimize (speed 0) (debug 3) (safety 3)))
(in-package :cl-openddr)

(defclass device ()
  ((id :initarg :id :accessor id)
   (confidence :initform nil :initarg :confidence :accessor confidence)))

(defmethod print-object ((obj device) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~A ~~~A" (id obj) (confidence obj))))

(defun lookup-string (string trie)
  (loop for c from 0 below (length string)
          for lookup = (lookup-in-trie string c trie)
       do
         (when lookup
           (let ((res (car (sort lookup #'> :key #'length))))
             (return-from lookup-string
               (values res c (+ c (length res))))))))

(defun trie-and-lookup (class)
  (let ((data (loop for x in (token-device-lookup-table
                              class) 
                 collect x)))
    (list
     (construct-trie 
      (mapcar (lambda (x) (string-downcase (car x))) data))
     (let ((hash (make-hash-table :test #'equalp)))
       (loop for (pattern . id) in data do (setf (gethash pattern hash) id))
       hash))))


(def-device-builder android-device-builder (user-agent)
  (let ((inside (string-downcase (get-inside-pattern user-agent))))
    (destructuring-bind (trie hash)
        '#.(trie-and-lookup "org.openddr.simpleapi.oddr.builder.device.AndroidDeviceBuilder")
        (multiple-value-bind (device start end)
            (lookup-string 
             inside
             trie)
          (declare (ignore start))
          (when device
            (let ((d (make-instance 'device :id (gethash device hash)
                                    :confidence 
                                    (cond ((scan "Build/.*" inside :start end)
                                           100)
                                          (t 90)))))
              d))))))


(def-device-builder ios-device-builder (user-agent)
  (when (or (not (contains-ios-devices user-agent))
            (contains-android user-agent)
            (contains-windows-phone user-agent))
    (return-from defbuilder))
  (let ((inside (string-downcase (get-inside-pattern user-agent))))
    (destructuring-bind (trie hash)
        '#.(trie-and-lookup "org.openddr.simpleapi.oddr.builder.device.IOSDeviceBuilder")
        (multiple-value-bind (device start end)
            (lookup-string 
             inside
             trie)
          (declare (ignore start))
          (when device
            (let ((d (make-instance 'device :id (gethash device hash)
                                    :confidence 
                                    (cond ((scan "Build/.*" inside :start end)
                                           100)
                                          (t 90)))))
              d))))))


(def-device-builder symbian-device-builder (user-agent)
  (when (not (contains-symbian user-agent)) 
    (return-from defbuilder))
  (let ((inside (string-downcase (get-inside-pattern user-agent))))
    (destructuring-bind (trie hash)
        '#.(trie-and-lookup "org.openddr.simpleapi.oddr.builder.device.SymbianDeviceBuilder")
        (multiple-value-bind (device start end)
            (lookup-string 
             inside
             trie)
          (declare (ignore start))
          (when device
            (let ((d (make-instance 'device :id (gethash device hash)
                                    :confidence 
                                    (cond ((scan "Build/.*" inside :start end)
                                           100)
                                          (t 90)))))
              d)))))
  )


(def-device-builder winphone-device-builder (user-agent)
  (when (not (contains-windows-phone user-agent)) 
    (return-from defbuilder))
  (let ((inside (string-downcase (get-inside-pattern user-agent))))
    (destructuring-bind (trie hash)
        '#.(trie-and-lookup "org.openddr.simpleapi.oddr.builder.device.WinPhoneDeviceBuilder")
        (multiple-value-bind (device start end)
            (lookup-string 
             inside
             trie)
          (declare (ignore start))
          (when device
            (let ((d (make-instance 'device :id (gethash device hash)
                                    :confidence 
                                    (cond ((scan "Build/.*" inside :start end)
                                           100)
                                          (t 90)))))
              d))))))

(def-device-builder twostep-device-builder (user-agent)
  (let ((id (lookup-twostep (string-downcase user-agent) #.(two-step-data-trie))))
      (when id
        (let ((d (make-instance 'device :id id
                                :confidence 
                                80)))
          d))))

(defun detect-device (user-agent)
  (loop for x in (list #'android-device-builder 
                       #'ios-device-builder
                       #'symbian-device-builder
                       #'winphone-device-builder
                       #'twostep-device-builder)
     for res = (funcall x user-agent)
     when res
     do (return-from detect-device res)
       ))