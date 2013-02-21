(in-package :cl-openddr)

(defun load-device-database (file)
  (let ((source (cxml:make-source (open file :element-type '(unsigned-byte 8)))))
    (loop for x = (multiple-value-list 
                   (klacks:find-element source "device"))
       while (not (equalp x '(nil)))
       collect (let ((id nil)
                     (parentid nil)
                     (res nil))
                 (KLACKS:MAP-ATTRIBUTES (lambda (namespace name* qname val bool)
                                          (when (string= name* "id")
                                            (setf id val))
                                          (when (string= name* "parentId")
                                            (setf parentid val)))
                                        source)
                 (loop with finished = nil
                      while (not finished)
                    do (multiple-value-bind (type b c d) 
                           (klacks:peek-next source)
                         (declare (ignore b d))
                         (when (and (eq type :end-element)
                                    (equalp c "device"))
                           (setf finished t))
                         (when (and (eq type :start-element)
                                    (equalp c "property"))
                           (let ((name nil)
                                 (value nil))
                             (KLACKS:MAP-ATTRIBUTES (lambda (namespace name* qname val bool)
                                                      (declare (ignore namespace qname bool))
                                                      (when (string= name* "name")
                                                        (setf name val))
                                                      (when (string= name* "value")
                                                        (setf value val)))
                                                    source)
                             (setf name
                                   (intern (string-upcase (cl-ppcre:regex-replace-all "_" name "-"))
                                           :keyword))
                             (cond ((ignore-errors (parse-integer value))
                                    (setf value (parse-integer value)))
                                   ((equalp value "true")
                                    (setf value t))
                                   ((equalp value "false")
                                    (setf value nil)))
                             (push name res)
                             (push value res)))))
                 (cons (sb-ext:string-to-octets id) (cons parentid (reverse res)))))))

;; (defvar *foo* nil)
;; (progn
;;   (time
;;    (setf *foo*
;;          (load-device-database "/home/asgeir/work/openddr/OpenDDR-Resources/resources/DeviceDataSource.xml")))
;;   nil)

;; (with-open-file (out #p"/tmp/file.lisp" :direction :output)
;;   (format out "~S" *foo*))


(defun transform-to-utf8 (data)
  (labels ((ts (string)
             (sb-ext:string-to-octets string :external-format :utf-8)))
    (list
     (ts (car data))
     (ts (cadr data))
     (loop for (key value . rest) on (cddr data) by #'cddr
           collect key
           collect (if (stringp value)
                       (ts value)
                       value)))))

(defvar *device-database*
  #.(let ((hash (make-hash-table :test #'equalp)))
      (loop for x in (read (open (asdf:system-relative-pathname :cl-openddr "device-data.data")))
         do (setf (gethash (car x) hash)
                  (cdr x))) 
    hash))

(defun device-lookup (device)
  (let ((seen nil))
    (loop for (key val . rest) on  (loop for key = device then parent
                                      for (parent . properties) = (gethash key *device-database*)
                                      while parent
                                      append properties) by #'cddr
       unless (member key seen)
       collect key
       unless (member key seen)
       collect (progn (push key seen)
                      val))))

(device-lookup "LT26i")
(device-lookup "Xperia Neo")
"10.47.243.118 - - [21/Feb/2013:09:03:21 +0100] \"GET /favicon.ico HTTP/1.1\" 404 447 \"http://10.47.243.42/\" \"Mozilla/5.0 (Linux; U; Android 2.3.4; nb-no; SonyEricssonMT15i Build/4.0.2.A.0.62) AppleWebKit/533.1 (KHTML, like Gecko) Version/4.0 Mobile Safari/533.1\""

(parse-string "()")

(cl-ppcre:scan '(:alternation 
                 (:register "abba")
                 (:register "ab")) "abbaaa")

"org.openddr.simpleapi.oddr.builder.device.AndroidDeviceBuilder"

(defun load-android-ids (file)
  (let ((source (cxml:make-source (open file :element-type '(unsigned-byte 8))))
        (res nil))
    (loop for x = (multiple-value-list 
                   (klacks:find-element source "builder"))
       while (not (equalp x '(nil)))
       do (let ((class nil))
                 (KLACKS:MAP-ATTRIBUTES (lambda (namespace name* qname val bool)
                                          (when (string= name* "class")
                                            (setf class val)))
                                        source)
                 (if (not (string= class "org.openddr.simpleapi.oddr.builder.device.AndroidDeviceBuilder"))
                     (klacks:peek-next source)
                     (progn 
                       (let ((id nil))
                         (loop with finished = nil
                            while (not finished)
                            do (multiple-value-bind (type b c d) 
                                   (klacks:peek-next source)
                                        ;(declare (ignore b d))
                                 (format t "~S~%" (list type b c d))
                                 (when (and (eq type :end-element)
                                            (equalp c "builder"))
                                   (setf finished t)
                                   ;(return-from 'load-android-ids)
                                   )
                                 (when (and (eq type :start-element)
                                            (equalp c "device"))
                                   (KLACKS:MAP-ATTRIBUTES (lambda (namespace name* qname val bool)
                                                            (declare (ignore namespace qname bool))
                                                            (when (string= name* "id")
                                                              (setf id val)))
                                                          source))
                                 (when (and (eq type :start-element)
                                            (equalp c "value"))
                                   (multiple-value-bind (type b c d) 
                                       (klacks:peek-next source)
                                     (declare (ignore c d))
                                     (assert (eq type :characters))
                                     (push (cons b id) res))))))))))
    (reverse res)
    ))
;(device-lookup "HTC One X+")
;(load-android-ids "/home/asgeir/work/OpenDDR-Resources/resources/BuilderDataSource.xml")

(defun token-device-lookup-reg ()
  (let ((lookups
         (load-android-ids "/home/asgeir/work/OpenDDR-Resources/resources/BuilderDataSource.xml")))
    (setf lookups (sort lookups #'> :key (lambda (x) (length (car x)))))
    (values 
     (create-scanner
      (cons 
       :alternation
       (loop for (l . id) in lookups
          collect 
          `(:register 
            (:sequence
             ,l
             (:GREEDY-REPETITION 0 1 :EVERYTHING)
             ))
          collect 
          `(:register 
            (:sequence
             ,l
             (:GREEDY-REPETITION 0 1 :EVERYTHING)
             "Build/")))))
     (lambda (starts)
       (when (position-if-not #'null starts)
         (cdr (nth (floor (position-if-not #'null starts) 2) lookups))))
     )))

(cl-ppcre:parse-string ".?Build/.*")
(multiple-value-bind (scanner reslookup)
    (token-device-lookup-reg)
  (time
   (progn
     (multiple-value-bind (a b c d)
         (scan scanner "Mozilla/5.0 (Linux; U; Android 2.3.4; nb-no; SonyEricssonMT15a Build/4.0.2.A.0.62) AppleWebKit/533.1 (KHTML, like Gecko) Version/4.0 Mobile Safari/533.1")
       (funcall reslookup c)))))


;; (time
;;  (length
;;   (loop for x being the hash-keys of *device-database*
;;      collect x)))
;(blah (list "EN" "EN dumming" "foobar" "abba" "abba EN"))

