(in-package :cl-openddr)

(def-layout-engine-builder android-mobile-browser-builder
    (user-agent layout-engine layout-engine-version hinted-width hinted-height)
  (when (or (not (contains-android user-agent))
            (not (has-mozilla-pattern user-agent))
            (user-agent-contains "Fennec" user-agent))
    (return-from defbuilder nil))
  (let ((identified (make-instance 'identified :vendor "Google" :model "Android Browser"))
        (confidence 70)
        (safari-reg ".*Safari/([0-9\\.]+).*?")
        (version-reg ".*Version/([0-9\\.]+).*?"))

    (if (scan version-reg user-agent)
        (register-groups-bind (version)
             (version-reg user-agent)
          (loop for x in (split '(:sequence #\.) version)
             for acc in '(#'(setf version-major) #'(setf version-minor) #'(setf version-micro) #'(setf version-nano))
             do (funcall acc x)))
        (progn
          (setf (version-minor identified) 0)
          (setf (version-major identified) 1)))

    (when layout-engine
      (setf (layout-engine identified) layout-engine)
      (setf (layout-engine-version identified) layout-engine-version)
      (when (string= layout-engine "AppleWebKit")
        (incf confidence 10)))

    (register-groups-bind (safari-version)
        (safari-reg user-agent)
      (setf (reference-browser identified) "Safari")
      (setf (reference-browser-version identified) safari-version))

    (setf (display-width identified) hinted-width)
    (setf (display-height identified) hinted-height)
    (setf (confidence identified) confidence)

    identified))
