(defun parse-val (val)
  (cond ((string= val "false")
         nil)
        ((string= val "true")
         t)
        ((ignore-errors (parse-integer val))
         (parse-integer val))
        (t val)))

(defun parse-key (key)
  (cl-ppcre:register-groups-bind (aspect key)
      ("^(.*):(.*)$" (string-upcase (cl-ppcre:regex-replace-all "_" key "-")))
    (cons (intern aspect :keyword) (intern key :keyword))))


(defun blah ()
  (with-open-file (str "devout.txt")
    (loop for user-agent = (read-line str nil nil)
       while user-agent
       collect
       (let ((keyvals (loop for x = (read-line str nil nil)
                         while (and x (not (string= x "")))
                         collect x)))
          
         (cons user-agent
               (loop for (key val .rest ) on keyvals by #'cddr
                  collect (cons (parse-key key)
                                (parse-val val)
                                )))
          
         ))))

(with-open-file (out "/tmp/out.txt" :direction :output)
  (format out "~S" (blah)))