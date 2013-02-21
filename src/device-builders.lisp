(declaim (optimize (speed 0) (debug 3) (safety 3)))
(in-package :cl-openddr)

(defun lookup-string (string)
  (let ((trie #.(construct-trie (loop for x in (token-device-lookup-table) collect (car x)))))
    (loop for c from 0 below (length string)
          for lookup = (lookup-in-trie string c trie)
       do
         (when lookup
           (let ((res (car (sort lookup #'> :key #'length))))
             (return-from lookup-string
               (values res c (+ c (length res)))))))))

;(time (lookup-string3 "Mozilla/5.0 (Linux; U; Android 4.0.3; nb-no; U9200 Build/HuaweiU9200) AppleWebKit/534.30 (KHTML, like Gecko) Version/4.0 Mobile Safari/534.30"))




