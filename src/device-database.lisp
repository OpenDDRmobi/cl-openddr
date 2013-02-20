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
