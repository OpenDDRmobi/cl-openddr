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
      (setf (layout-engine-of identified) layout-engine)
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

(defun call-layout-engine-builder (builder user-agent)
  (multiple-value-bind (le lev)
      (layout-engine user-agent)
    (multiple-value-bind (width height)
        (hinted-resolution 
         user-agent)
      (funcall builder
       user-agent
       le lev 
       width height))))


(def-layout-engine-builder chrome-browser-builder (user-agent layout-engine layout-engine-version hinted-width hinted-height)
  (when (or (not (cl-ppcre:scan "Chrome" user-agent))
            (not (has-mozilla-pattern user-agent)))
    (return-from defbuilder nil))
  (let ((identified (make-instance 'identified :vendor "Google" :model "Chrome"))
        (confidence 60)
        )
    (if (scan ".*Chrome.([0-9a-z\\.b]+).*" user-agent)
        (cl-ppcre:register-groups-bind (version) (".*Chrome.([0-9a-z\\.b]+).*" user-agent)
          (let ((version (split '(:sequence ".") version)))
            (when (car version)
              (if (string= (car version) "")
                  (setf (version-major identified) "1")
                  (setf (version-major identified) (car version))))
            (when (cdr version)
              (incf confidence 10)
              (setf (version-minor identified) (cadr version)))
            (when (cddr version)
              (setf (version-micro identified) (caddr version)))
            (when (cdddr version)
              (setf (version-nano identified) (cadddr version)))))
        (setf (version-major identified) "1"))

    (when layout-engine
      (setf (layout-engine-of identified) layout-engine)
      (setf (layout-engine-version identified) layout-engine-version)
      (when (string= layout-engine "KHTML")
        (incf confidence 10)))

    (register-groups-bind (safari-version)
        (".*Safari/([0-9\\.]+).*?" user-agent)
      (incf confidence 10)
      (setf (reference-browser identified) "Safari")
      (setf (reference-browser-version identified) safari-version))

    (setf (display-width identified) hinted-width)
    (setf (display-height identified) hinted-height)
    (setf (confidence identified) confidence)

    identified))

(def-layout-engine-builder internet-explorer-browser-builder (user-agent layout-engine layout-engine-version hinted-width hinted-height)
  (when (or 
         (not (has-mozilla-pattern user-agent))
         (not (contains-msie user-agent))
         (scan 
          ".*Windows.?(?:(?:CE)|(?:Phone)).*"
          user-agent))
    (return-from defbuilder nil))
  (let ((identified (make-instance 'identified :vendor "Microsoft" :model "Internet Explorer"))
        (confidence 60))
    (if (scan ".*MSIE.([0-9\\.b]+).*" user-agent)
        (cl-ppcre:register-groups-bind (version) (".*MSIE.([0-9\\.b]+).*" user-agent)
          (let ((version (split '(:sequence ".") version)))
            (when (car version)
              (if (string= (car version) "")
                  (setf (version-major identified) "1")
                  (setf (version-major identified) (car version))))
            (when (cdr version)
              (incf confidence 10)
              (setf (version-minor identified) (cadr version)))
            ))
        (setf (version-major identified) "1"))

    (when layout-engine
      (setf (layout-engine-of identified) layout-engine)
      (setf (layout-engine-version identified) layout-engine-version)
      (when (string= layout-engine "Trident")
        (incf confidence 10)))

    (when (scan ".*\\.NET.CLR.*" user-agent)
      (incf confidence 10))
    (setf (display-width identified) hinted-width)
    (setf (display-height identified) hinted-height)
    (setf (confidence identified) confidence)
    identified))


;; (call-layout-engine-builder 
;;  #'CHROME-BROWSER-BUILDER 
;;  "User-Agent:Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.17 (KHTML, like Gecko) Chrome/24.0.1312.56 Safari/537.17")

 ;; (call-layout-engine-builder 
 ;; #'internet-explorer-browser-builder
 ;; "Mozilla/5.0 (compatible; MSIE 7.0; Windows NT 5.0; Trident/4.0; FBSMTWB; .NET CLR 2.0.34861; .NET CLR 3.0.3746.3218; .NET CLR 3.5.33652; msn OptimizedIE8;ENUS)")

