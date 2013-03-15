(declaim (optimize (speed 0) (debug 3) (safety 3)))
(in-package :cl-openddr)

(defun populate-versions (identified version-string &key (c-boost '(0 0 0 0)) 
                          (split-reg (cl-ppcre:create-scanner '(:sequence "."))))
  (let ((conf-boost 0))
    (loop for x in (split split-reg version-string)
       for writer in  (list #'(setf version-major) #'(setf version-minor) #'(setf version-micro) #'(setf version-nano))
       for c in c-boost
       do (progn
            (incf conf-boost c)
            (funcall writer x identified)))
    conf-boost))

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
          (incf confidence (populate-versions identified version)))
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


(def-layout-engine-builder blackberry-browser-builder (user-agent layout-engine layout-engine-version hinted-width hinted-height)
  (when (or 
         (not (user-agent-contains "BlackBerry" user-agent)))
    (return-from defbuilder nil))
  (let ((identified (make-instance 'identified :vendor "RIM" :model "BlackBerry"))
        (confidence 50))
    (if (scan ".*(?:(?:Version)|(?:[Bb]lack.?[Bb]erry.?(?:[0-9a-z]+)))/([0-9\\.]+).*"
              user-agent)
        (cl-ppcre:register-groups-bind (v1 v2) (".*(?:(?:Version)|(?:[Bb]lack.?[Bb]erry.?(?:[0-9a-z]+)))/([0-9\\.]+).*" user-agent)
          (let ((totalversion (if (and v1 (not (string= v1 ""))) v1 v2)))
            (incf confidence (populate-versions identified totalversion
                                                  :c-boost '(0 10 0 0)))))
        (setf (version-major identified) "1"))

    (when layout-engine
      (setf (layout-engine-of identified) layout-engine)
      (setf (layout-engine-version identified) layout-engine-version)
      (when (string= layout-engine "AppleWebKit")
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


(def-layout-engine-builder chrome-mobile-browser-builder (user-agent layout-engine layout-engine-version hinted-width hinted-height)
  (unless (and
           (or (contains-android user-agent)
               (contains-ios-devices user-agent))
           (or (user-agent-contains "Chrome" user-agent)
               (user-agent-contains "CriOS" user-agent))
           (has-mozilla-pattern user-agent)
           (user-agent-contains "Mobile" user-agent))
    (return-from defbuilder nil))
  (let ((identified (make-instance 'identified :vendor "Google" :model "Chrome"))
        (confidence 70))
    (if (scan "(?:.*Chrome/([0-9\\.]+).*?)|(?:.*CriOS/([0-9\\.]+).*?)" user-agent)
        (cl-ppcre:register-groups-bind (v1 v2) 
            ("(?:.*Chrome/([0-9\\.]+).*?)|(?:.*CriOS/([0-9\\.]+).*?)" 
             user-agent)
          (let ((version (if (and v1 (not (equalp v1 ""))) v1 v2)))
            (incf confidence (populate-versions identified version))))
        (progn
          (setf (version-major identified) "18")
          (setf (version-minor identified) "0")))

    (when layout-engine
      (setf (layout-engine-of identified) layout-engine)
      (setf (layout-engine-version identified) layout-engine-version)
      (when (string= layout-engine "AppleWebKit")
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




(def-layout-engine-builder dolfin-browser-builder (user-agent layout-engine layout-engine-version hinted-width hinted-height)
  (unless (and
           (user-agent-contains "Dolfin" user-agent)
           (scan ".*?(?:(?:Dolfin/))([0-9\\.]+).*?" user-agent))
    (return-from defbuilder nil))
  (let ((identified (make-instance 'identified :vendor "Samsung" :model "Dolfin"))
        (confidence 60))
    (register-groups-bind (version) (".*?(?:(?:Dolfin/))([0-9\\.]+).*?" user-agent)
      (incf confidence 
            (populate-versions identified version :c-boost '(0 10 0 0))))
    

    (when layout-engine
      (setf (layout-engine-of identified) layout-engine)
      (setf (layout-engine-version identified) layout-engine-version)
      (when (string= layout-engine "AppleWebKit")
        (incf confidence 10)))

    (setf (display-width identified) hinted-width)
    (setf (display-height identified) hinted-height)
    (setf (confidence identified) confidence)

    identified))

(def-layout-engine-builder fennec-browser-builder (user-agent layout-engine layout-engine-version hinted-width hinted-height)
  (unless (and
           (or (user-agent-contains "Fennec" user-agent)
               (user-agent-contains "Firefox" user-agent))
           (has-mozilla-pattern user-agent)
           (user-agent-contains "Mobile" user-agent))
    (return-from defbuilder nil))
  (let ((identified (make-instance 'identified :vendor "Mozilla" :model "Firefox Mobile"))
        (confidence 60))
    
    (register-groups-bind (version) (".*Fennec/([0-9a-z\\.\\-]+)" user-agent)
      (incf confidence
            (populate-versions identified version :c-boost '(0 10 0 0))))
    (register-groups-bind (version) (".*Firefox.([0-9a-z\\.b]+).*" user-agent)
      (incf confidence
            (populate-versions identified version :c-boost '(0 10 0 0)))
      (setf (reference-browser identified) "Firefox")
      (setf (reference-browser-version identified) version))
    
    (when layout-engine
      (setf (layout-engine-of identified) layout-engine)
      (setf (layout-engine-version identified) layout-engine-version)
      (when (string= layout-engine "Gecko")
        (incf confidence 10)))
    

    (setf (display-width identified) hinted-width)
    (setf (display-height identified) hinted-height)
    (setf (confidence identified) confidence)

    identified))

(def-layout-engine-builder firefox-browser-builder (user-agent layout-engine layout-engine-version hinted-width hinted-height)
  (unless (and
           (user-agent-contains "Firefox" user-agent)
           (has-mozilla-pattern user-agent)
           (not (user-agent-contains "Fennec" user-agent))
           (user-agent-contains ".*Gecko/([0-9]+).*Firefox.*" user-agent))
    (return-from defbuilder nil))
  (let ((identified (make-instance 'identified :vendor "Mozilla" :model "Firefox"))
        (confidence 60))
    (register-groups-bind (version) (".*Firefox.([0-9a-z\\.b]+).*" user-agent)
      (incf confidence
            (populate-versions identified version :c-boost '(0 10 0 0))))
    (when layout-engine
      (setf (layout-engine-of identified) layout-engine)
      (setf (layout-engine-version identified) layout-engine-version)
      (when (string= layout-engine "Gecko")
        (incf confidence 10)))

    (setf (display-width identified) hinted-width)
    (setf (display-height identified) hinted-height)
    (setf (confidence identified) confidence)

    identified))


(def-layout-engine-builder ie-mobile-browser-builder (user-agent layout-engine layout-engine-version hinted-width hinted-height)
  (unless (and            
           (contains-windows-phone user-agent)
           (scan 
            ".*Windows.?(?:(?:CE)|(?:Phone)).*"
            user-agent))
    (return-from defbuilder nil))
  (let ((identified (make-instance 'identified :vendor "Microsoft" :model "IEMobile"))
        (confidence 40))
    (if (user-agent-contains "MSIEMobile" user-agent)
        (incf confidence 10))
    (when  (has-mozilla-pattern user-agent)
      (incf confidence 10))
    (register-groups-bind (version) (".*[^MS]IEMobile.([0-9\\.]+).*?" user-agent)
      (incf confidence
            (populate-versions identified version :c-boost '(0 10 0 0))))
    
    (register-groups-bind (ver) (".*MSIE.([0-9\\.]+).*" user-agent)
      (setf (reference-browser identified) "MSIE")
      (setf (reference-browser-version identified) ver)
      (incf confidence 10))
    (register-groups-bind (ver) (".*MSIEMobile.([0-9\\.]+).*" user-agent)
      (setf (layout-engine-of identified) "MSIEMobile")
      (setf (layout-engine-version identified) ver)
      (incf confidence 10))
    
    (when layout-engine
      (setf (layout-engine-of identified) layout-engine)
      (setf (layout-engine-version identified) layout-engine-version)
      (when (string= layout-engine "Trident")
        (incf confidence 10)))

    (setf (display-width identified) hinted-width)
    (setf (display-height identified) hinted-height)
    (setf (confidence identified) confidence)
    identified))


(def-layout-engine-builder jasmine-browser-builder (user-agent layout-engine layout-engine-version hinted-width hinted-height)
  (unless (and
           (user-agent-contains "Jasmine" user-agent)
           (scan ".*?(?:(?:Jasmine/))([0-9\\.]+).*?" user-agent))
    (return-from defbuilder nil))
  (let ((identified (make-instance 'identified :vendor "Samsung" :model "Jasmine"))
        (confidence 60))
    (register-groups-bind (version) (".*?(?:(?:Jasmine/))([0-9\\.]+).*?" user-agent)
      (incf confidence 
            (populate-versions identified version :c-boost '(0 10 0 0))))
    

    (when layout-engine
      (setf (layout-engine-of identified) layout-engine)
      (setf (layout-engine-version identified) layout-engine-version))

    (setf (display-width identified) hinted-width)
    (setf (display-height identified) hinted-height)
    (setf (confidence identified) confidence)

    identified))


(def-layout-engine-builder konqueror-browser-builder (user-agent layout-engine layout-engine-version hinted-width hinted-height)
  (unless (and
           (user-agent-contains "Konqueror" user-agent)
           (has-mozilla-pattern user-agent))
    (return-from defbuilder nil))
  (let ((identified (make-instance 'identified :vendor "KDE" :model "Konqueror"))
        (confidence 60))
    (register-groups-bind (version) (".*Konqueror/([0-9a-z\\.\\-]+).*" user-agent)
      (incf confidence 
            (populate-versions identified version :c-boost '(0 10 0 0))))
    

    (when layout-engine
      (setf (layout-engine-of identified) layout-engine)
      (setf (layout-engine-version identified) layout-engine-version)
      (when (string= layout-engine "KHTML")
        (incf confidence 10)))

    (setf (display-width identified) hinted-width)
    (setf (display-height identified) hinted-height)
    (setf (confidence identified) confidence)

    identified))


(def-layout-engine-builder netfront-browser-builder (user-agent layout-engine layout-engine-version hinted-width hinted-height)
  (unless (and
           (or 
            (scan "(?i).*netfront.*" user-agent)
            (scan "ACS-NF" user-agent)
            (scan "NF-Browser" user-agent))
           (scan ".*?(?:(?:Net[Ff]ront)|(?:ACS-NF)|(?:NF-Browser))[/ ]?[/ ]?(?:WAP)?([0-9\\.]+).*?" 
                 user-agent)
           )
    (return-from defbuilder nil))
  (let ((identified (make-instance 'identified :vendor "Access" :model "NetFront"))
        (confidence 60))
    (register-groups-bind (version) 
        (".*?(?:(?:Net[Ff]ront)|(?:ACS-NF)|(?:NF-Browser))[/ ]?[/ ]?(?:WAP)?([0-9\\.]+).*?" 
         user-agent)
      (incf confidence 
            (populate-versions identified version :c-boost '(0 10 0 0))))
    
    (setf (display-width identified) hinted-width)
    (setf (display-height identified) hinted-height)
    (setf (confidence identified) confidence)

    identified))

(def-layout-engine-builder nokia-browser-builder (user-agent layout-engine layout-engine-version hinted-width hinted-height)
  (unless (and
           (or 
            (user-agent-contains "Nokia" user-agent)
            (user-agent-contains "NokiaBrowser" user-agent)
            (user-agent-contains "BrowserNG" user-agent)
            (user-agent-contains "Series60" user-agent))
           (or
            (has-mozilla-pattern user-agent)
            (user-agent-contains "SymbianOS" user-agent)
            (user-agent-contains "Symbian/3" user-agent)
            (user-agent-contains "Nokia" user-agent)))
    (return-from defbuilder nil))
  (let ((identified (make-instance 'identified :vendor "Nokia" :model "Nokia Browser"))
        (confidence 50))
    (register-groups-bind (version) 
        (".*(?:(?:BrowserNG)|(?:NokiaBrowser))/([0-9\\.]+).*"
         user-agent)
      (incf confidence 
            (populate-versions identified version :c-boost '(0 10 0 0))))
    
    (when layout-engine
      (setf (layout-engine-of identified) layout-engine)
      (setf (layout-engine-version identified) layout-engine-version)
      (when (string= layout-engine "AppleWebKit")
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


(def-layout-engine-builder obigo-browser-builder (user-agent layout-engine layout-engine-version hinted-width hinted-height)
  (unless (and
           (scan "((?i).*obigo.*)|((?i).*teleca.*)" user-agent))
    (return-from defbuilder nil))
  (let ((identified (make-instance 'identified :vendor "Obigo" :model "Obigo Browser"))
        (confidence 60))
    
    (register-groups-bind (version) 
        (".*(?:(?:BrowserNG)|(?:NokiaBrowser))/([0-9\\.]+).*"
         user-agent)
      (incf confidence 
            (populate-versions identified version :c-boost '(0 10 0 0))))
    (register-groups-bind (version)
        (".*?(?:(?:ObigoInternetBrowser/)|(?:Obigo Browser )|(?:[Oo]bigo[- ][Bb]rowser/))([0-9A-Z\\.]+).*?"
         user-agent)
      (incf confidence 
            (populate-versions identified version)))
    (register-groups-bind (version)
        (".*?(?:(?:Browser/Obigo)|(?:OBIGO[/_-])|(?:Obigo[-/ ]))([0-9A-Z\\.]+).*?"
         user-agent)
      (incf confidence 
            (populate-versions identified version)))
    (register-groups-bind (version)
        (".*?(?:(?:Obigo[Il]nternetBrowser/)|(?:Obigo Browser )|(?:[Oo]bigo[- ][Bb]rowser/))([0-9A-Zacqv\\.]+).*?;"
         user-agent)
      (incf confidence 
            (populate-versions identified version)))
    (register-groups-bind (version)
        (".*?(?:(?:[Bb]rowser/[Oo]bigo)|(?:OBIGO[/_-])|(?:Obigo[-/ ]))([0-9A-Zacqv\\.]+).*?"
         user-agent)
      (incf confidence 
            (populate-versions identified version)))
    (register-groups-bind (version)
        (".*?(?:(?:[Tt]eleca Q))([0-9A-Zacqv\\.]+).*?;"
         user-agent)
      (setf (model identified) "Teleca-Obigo")
      (incf confidence 
            (populate-versions identified version)))
    
    (when layout-engine
      (setf (layout-engine-of identified) layout-engine)
      (setf (layout-engine-version identified) layout-engine-version))

    (setf (display-width identified) hinted-width)
    (setf (display-height identified) hinted-height)
    (setf (confidence identified) confidence)

    identified))



(def-layout-engine-builder openwave-browser-builder (user-agent layout-engine layout-engine-version hinted-width hinted-height)
  (unless (and
           (scan "(?i).*openwave.*" user-agent)
           (scan "(?i).*openwave[/ ]?([0-9\\.]+).*?" user-agent))
    (return-from defbuilder nil))
  (let ((identified (make-instance 'identified :vendor "Openwave" :model "Openwave"))
        (confidence 60))
    (register-groups-bind (version) 
        ("(?i).*openwave[/ ]?([0-9\\.]+).*?"
         user-agent)
      (incf confidence 
            (populate-versions identified version :c-boost '(0 10 0 0))))
    
    (when layout-engine
      (setf (layout-engine-of identified) layout-engine)
      (setf (layout-engine-version identified) layout-engine-version))

    (setf (display-width identified) hinted-width)
    (setf (display-height identified) hinted-height)
    (setf (confidence identified) confidence)

    identified))

(def-layout-engine-builder opera-builder (user-agent layout-engine layout-engine-version hinted-width hinted-height)
  (unless (and
           (has-opera-pattern user-agent)
           (scan ".* Opera [0-9\\.]+.*" user-agent)
           (not (user-agent-contains "Opera Mini" user-agent)))
    (return-from defbuilder nil))
  (let ((identified (make-instance 'identified :vendor "Opera" 
                                   :model 
                                   (cond ((user-agent-contains "Mobi" user-agent)
                                          "Opera Mobile")
                                         ((user-agent-contains "Tablet" user-agent)
                                          "Opera Tablet")
                                         (t
                                          "Opera"))))
        (confidence 60))
    (register-groups-bind (version) 
        (".* Opera ([0-9\\.]+).*"
         user-agent)
      (incf confidence 
            (populate-versions identified version :c-boost '(0 10 0 0))))
    
    (when layout-engine
      (setf (layout-engine-of identified) layout-engine)
      (setf (layout-engine-version identified) layout-engine-version)
      (when (string= layout-engine "Presto")
        (incf confidence 10)))
    (loop for x in (cl-ppcre:split ";" (get-inside-pattern user-agent))
         do (register-groups-bind (version)
                ("Opera Mobi/(.*)" x)
              (setf (reference-browser identified) "Opera Mobi")
              (setf (reference-browser-version identified) version)
              (incf confidence 10)))
    
    (setf (display-width identified) hinted-width)
    (setf (display-height identified) hinted-height)
    (setf (confidence identified) confidence)

    identified))



(def-layout-engine-builder opera-mini-builder (user-agent layout-engine layout-engine-version hinted-width hinted-height)
  (unless (and
           (user-agent-contains "Opera Mini" user-agent)
           (scan ".*?Opera Mini/(?:att/)?v?((\\d+)\\.(\\d+)(?:\\.(\\d+))?(?:\\.(\\d+))?).*?" user-agent))
    (return-from defbuilder nil))
  (let ((identified (make-instance 'identified :vendor "Opera" 
                                   :model 
                                   "Opera Mini"))
        (confidence 60))
    (register-groups-bind (version) 
        (".*?Opera Mini/(?:att/)?v?((\\d+)\\.(\\d+)(?:\\.(\\d+))?(?:\\.(\\d+))?).*?"
         user-agent)
      (incf confidence 
            (populate-versions identified version)))
    (when (has-opera-pattern user-agent)
      (setf (reference-browser identified) "Opera")
      (register-groups-bind (v1 v2 v3)
          ("(.*?)((?:Mozilla)|(?:Opera))[/ ](\\d+\\.\\d+).*?\\(((?:.*?)(?:.*?\\(.*?\\))*(?:.*?))\\)(.*)"
           user-agent)
        (declare (ignore v1 v2))
        (setf (reference-browser-version identified) v3)))
    
    (register-groups-bind (build)
        (".*?Opera Mini/(?:att/)?v?.*?/(.*?);.*" user-agent)
      (setf (version-build identified) build))

    (when layout-engine
      (setf (layout-engine-of identified) layout-engine)
      (setf (layout-engine-version identified) layout-engine-version)
      (when (string= layout-engine "Presto")
        (incf confidence 10)))
    
    (setf (display-width identified) hinted-width)
    (setf (display-height identified) hinted-height)
    (setf (confidence identified) confidence)

    identified))


(def-layout-engine-builder polaris-browser-builder (user-agent layout-engine layout-engine-version hinted-width hinted-height)
  (unless (and
           (user-agent-contains "Polaris" user-agent)
           (scan ".*?(?:(?:Polaris)|(?:POLARIS))[/ ](?:v)?([0-9\\.]+).*?" user-agent)
           )
    (return-from defbuilder nil))
  (let ((identified (make-instance 'identified :vendor "Infraware" :model "Polaris"))
        (confidence 60))
    (register-groups-bind (version) (".*?(?:(?:Polaris)|(?:POLARIS))[/ ](?:v)?([0-9\\.]+).*?" user-agent)
      (incf confidence 
            (populate-versions identified version :c-boost '(0 10 0 0))))
    

    (when layout-engine
      (setf (layout-engine-of identified) layout-engine)
      (setf (layout-engine-version identified) layout-engine-version))

    (setf (display-width identified) hinted-width)
    (setf (display-height identified) hinted-height)
    (setf (confidence identified) confidence)

    identified))


(def-layout-engine-builder safari-browser-builder (user-agent layout-engine layout-engine-version hinted-width hinted-height)
  (unless (and
           (user-agent-contains "Safari" user-agent)
           (not (user-agent-contains "Mobile" user-agent)))
    (return-from defbuilder nil))
  (let ((identified (make-instance 'identified :vendor "Apple" :model "Safari"))
        (confidence 60))
    (register-groups-bind (version) (".*Version/([0-9\\.]+).*"
                                     user-agent)
      (incf confidence 
            (populate-versions identified version :c-boost '(0 10 0 0))))
    

    (when layout-engine
      (setf (layout-engine-of identified) layout-engine)
      (setf (layout-engine-version identified) layout-engine-version)
      (when (string= layout-engine "Gecko")
        (incf confidence 10)))

    (setf (display-width identified) hinted-width)
    (setf (display-height identified) hinted-height)
    (setf (confidence identified) confidence)

    identified))


(def-layout-engine-builder safari-mobile-browser-builder (user-agent layout-engine layout-engine-version hinted-width hinted-height)
  (unless (and
           (contains-ios-devices user-agent))
    (return-from defbuilder nil))
  (let ((identified (make-instance 'identified :vendor "Apple" :model "Mobile Safari"))
        (confidence 60))
    (register-groups-bind (version) (".*Version/([0-9\\.]+).*?"
                                     user-agent)
      (incf confidence 
            (populate-versions identified version :c-boost '(0 10 0 0))))
    

    (when layout-engine
      (setf (layout-engine-of identified) layout-engine)
      (setf (layout-engine-version identified) layout-engine-version)
      (when (string= layout-engine "AppleWebKit")
        (incf confidence 10)))
    (register-groups-bind (v)
          (".*Safari/([0-9\\.]+).*?" user-agent)
        (setf (reference-browser identified) "Safari")
        (setf (reference-browser-version identified) v)
        (incf confidence 10))
    
    (setf (display-width identified) hinted-width)
    (setf (display-height identified) hinted-height)
    (setf (confidence identified) confidence)

    identified))

(def-layout-engine-builder semc-browser-builder (user-agent layout-engine layout-engine-version hinted-width hinted-height)
  (unless (and
           (user-agent-contains "SEMC" user-agent)
           (scan ".*SEMC?(?:(?:-Browser/)|(?:-BROWSER/)|(?:/NewsReader/)|(?:-Java/))(?:Symbian/)?([0-9\\.RA]+).*?" 
                 user-agent))
    (return-from defbuilder nil))
  (let ((identified (make-instance 'identified :vendor "SonyEricsson" :model "Mobile Browser"))
        (confidence 60))
    (register-groups-bind (version) (".*SEMC?(?:(?:-Browser/)|(?:-BROWSER/)|(?:/NewsReader/)|(?:-Java/))(?:Symbian/)?([0-9\\.RA]+).*?" 
                                     user-agent)
      (incf confidence 
            (populate-versions identified version :c-boost '(0 10 0 0))))
    

    (when layout-engine
      (setf (layout-engine-of identified) layout-engine)
      (setf (layout-engine-version identified) layout-engine-version))
    
    (setf (display-width identified) hinted-width)
    (setf (display-height identified) hinted-height)
    (setf (confidence identified) confidence)
    identified))


(def-layout-engine-builder silk-browser-builder (user-agent layout-engine layout-engine-version hinted-width hinted-height)
  (unless (and
           (has-mozilla-pattern user-agent)
           (user-agent-contains "Silk-Accelerated" user-agent))
    (return-from defbuilder nil))
  (let ((identified (make-instance 'identified :vendor "Amazon" :model "Silk"))
        (confidence 60))
    (register-groups-bind (version) (".*Silk/([0-9a-z\\.\\-]+)"
                                     (get-inside-pattern user-agent))
      (incf confidence 
            (populate-versions identified version :c-boost '(0 10 0 0)
                               :split-reg (cl-ppcre:create-scanner '(:ALTERNATION "." "-")))))
    
    (when layout-engine
      (setf (layout-engine-of identified) layout-engine)
      (setf (layout-engine-version identified) layout-engine-version)
      (when (string= layout-engine "AppleWebKit")
        (incf confidence 10)))
    
    (when (contains-android user-agent)
      (setf (reference-browser identified) "Android Browser")
      (cl-ppcre:register-groups-bind (v) (".*Version/([0-9\\.]+).*?" user-agent)
        (setf (reference-browser-version identified) v)
        (incf confidence 5)))
    (when (and (user-agent-contains "Safari" user-agent)
               (user-agent-contains "Mobile" user-agent))
      (setf (reference-browser identified) "Safari")
      (cl-ppcre:register-groups-bind (v) (".*Version/([0-9\\.]+).*?" user-agent)
        (setf (reference-browser-version identified) v)
        (incf confidence 5)))

    (setf (display-width identified) hinted-width)
    (setf (display-height identified) hinted-height)
    (setf (confidence identified) confidence)

    identified))

(def-layout-engine-builder up-browser-builder (user-agent layout-engine layout-engine-version hinted-width hinted-height)
  (unless (and
           (user-agent-contains "UP.Browser" user-agent)
           (scan ".*?(?:(?:UP\\.Browser))[/ ]?(?:WAP)?([0-9\\.abcd]+).*?" user-agent))
    (return-from defbuilder nil))
  (let ((identified (make-instance 'identified :vendor "Openwave" :model "UP.Browser"))
        (confidence 60))
    (register-groups-bind (version) (".*?(?:(?:UP\\.Browser))[/ ]?(?:WAP)?([0-9\\.abcd]+).*?"
                                     user-agent)
      (incf confidence 
            (populate-versions identified version :c-boost '(0 0 0 0))))
    
    (when layout-engine
      (setf (layout-engine-of identified) layout-engine)
      (setf (layout-engine-version identified) layout-engine-version))

    (setf (display-width identified) hinted-width)
    (setf (display-height identified) hinted-height)
    (setf (confidence identified) confidence)

    identified))

(def-layout-engine-builder webos-browser-builder (user-agent layout-engine layout-engine-version hinted-width hinted-height)
  (unless (and
           (user-agent-contains "webOSBrowser" user-agent)
           (scan ".*webOSBrowser/([0-9\\.]+).*?"  user-agent))
    (return-from defbuilder nil))
  (let ((identified (make-instance 'identified :vendor "Openwave" :model "UP.Browser"))
        (confidence 60))
    (register-groups-bind (version) (".*?(?:(?:UP\\.Browser))[/ ]?(?:WAP)?([0-9\\.abcd]+).*?"
                                     user-agent)
      (incf confidence 
            (populate-versions identified version :c-boost '(0 0 0 0))))
    
    (when layout-engine
      (setf (layout-engine-of identified) layout-engine)
      (setf (layout-engine-version identified) layout-engine-version))

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

(call-layout-engine-builder 
 #'android-mobile-browser-builder
 "10.47.243.118 - - [21/Feb/2013:09:03:21 +0100] \"GET /favicon.ico HTTP/1.1\" 404 447 \"http://10.47.243.42/\" \"Mozilla/5.0 (Linux; U; Android 2.3.4; nb-no; SonyEricssonMT15i Build/4.0.2.A.0.62) AppleWebKit/533.1 (KHTML, like Gecko) Version/4.0 Mobile Safari/533.1\"")