(in-package :cl-openddr)

(defun hinted-resolution (user-agent)
  (cl-ppcre:register-groups-bind (width height)
        ("^.*([0-9][0-9][0-9]+)[*Xx]([0-9][0-9][0-9]+).*$" user-agent)
    (return-from hinted-resolution
      (values (parse-integer width) (parse-integer height))))
  (macrolet ((match-res (regexp width height)
                 `(when (cl-ppcre:scan ,regexp
                                       user-agent)
                    (return-from hinted-resolution 
                      (values ,width ,height)))))
    (match-res ".*FWVGA.*" 480 854)
    (match-res ".*WVGA.*" 480 800)
    (match-res ".*WXGA.*" 240 400)
    (values nil nil)))


(defun layout-engine (user-agent)
  (macrolet ((matcher (regexp layout-engine)
               (check-type layout-engine string)
               `(cl-ppcre:register-groups-bind (version)
                    (,regexp user-agent)
                  (return-from layout-engine
                    (values ,layout-engine version)))))
    (matcher
     ".*AppleWebKit/([0-9\\.]+).*?" "AppleWebKit")
    (matcher
     ".*Presto/([0-9\\.]+).*?" "Presto")
    (matcher 
     ".*Gecko/([0-9\\.]+).*?" "Gecko")
    (matcher 
     ".*Trident/([0-9\\.]+).*?" "Trident")
    (matcher 
     ".*KHTML/([0-9\\.]+).*?" "KHTML")))


