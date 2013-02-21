(declaim (optimize (speed 3) (debug 0) (safety 0)))
(in-package :cl-openddr)

(defun construct-trie (strings &optional (accum ""))
  (setf strings (sort strings #'string<))
  (let ((buckets (make-hash-table)))
    (loop for x in strings
       do (push x
                (gethash (char x 0) buckets)))
    (let ((res 
           (loop for x being the hash-keys of buckets
              collect (list
                       x
                       (if (find-if (lambda (x) (= (length x) 1)) (gethash x buckets))
                           t
                           nil)
                       (construct-trie
                         (loop for w in (gethash x buckets)
                            unless (= (length w) 1)
                            collect (subseq w 1))
                         (format nil "~A~A" accum x))
                       ))))
      (make-array (length res)
                  :initial-contents res))))

(defun lookup-in-trie (string position trie)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (simple-string string))
  (let ((len (length string)))
    (labels ((inner (position trie path accum)
               (declare (fixnum position))
               (if (>= position len)
                   accum
                   (let ((res (find (char string position) trie :key #'car)))
                     (cond ((null res)
                            accum)
                           ((cadr res)
                            (inner (1+ position) (caddr res)
                                   (cons (car res) path) 
                                   (cons (cons (car res) path) 
                                         accum)))
                           ((not (cadr res))
                            (inner (1+ position) 
                                   (caddr res)
                                   (cons (car res) path) 
                                   accum)))))))
      (loop for x in (inner position trie nil nil)
         collect (concatenate 'string (reverse x))
           ))))
