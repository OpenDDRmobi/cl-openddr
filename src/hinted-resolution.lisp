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

(defun detect-browser (user-agent)
  (multiple-value-bind (width height)
      (hinted-resolution user-agent)
    (multiple-value-bind (layout-engine layout-engine-version)
        (layout-engine user-agent)
      (loop for (id . func) in (remove-duplicates *browser-builders* :key #'car :from-end t)
            for res = (funcall func user-agent layout-engine layout-engine-version width height)
         do (when res
              (return-from detect-browser res))))))

;; (detect-browser "Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.8.1.13) Gecko/20080311 (Debian-1.8.1.13+nobinonly-0ubuntu1) Kazehakase/0.5.2" )
;; (firefox-browser-builder "Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.8.1.13) Gecko/20080311 (Debian-1.8.1.13+nobinonly-0ubuntu1) Kazehakase/0.5.2" nil nil nil nil)
;; (detect-browser
;;  "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; KKman2.0)")

;; (device-lookup "MOT-v600")
;; (detect-device
;;  "MOT-V600/0B.09.38R MIB/2.2 Profile/MIDP-2.0 Configuration/CLDC-1.0")
;; (device-lookup 
;;  "MOT-V600/0B.09.38R MIB/2.2 Profile/MIDP-2.0 Configuration/CLDC-1.0")

;; (detect-device
;;  "Nokia2700c-2/2.0 (07.80) Profile/MIDP-2.1 Configuration/CLDC-1.1 nokia2700c-2/UC Browser7.7.1.88/69/444 UNTRUSTED/1.0")
