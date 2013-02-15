;;; qimt - A HTML toolkit for Common Lisp
;;; Copyright (C) 2013 Eivind Bergem <bergem@copyleft.no>
;;; Copyright (C) 2013 Asgeir Bjørlykke <asgeir@copyleft.no>
;;;



(asdf:defsystem :cl-openddr
  :version "0"
  :serial t
  :depends-on (:cl-ppcre)
  :components ((:file "package")
               (:file "base")
               (:file "hinted-resolution")
               (:file "layout-engine-builders")
               ))
