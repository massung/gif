(defpackage :gif-asd
  (:use :cl :asdf))

(in-package :gif-asd)

(defsystem :gif
  :name "gif"
  :version "1.0"
  :author "Jeffrey Massung"
  :license "Apache 2.0"
  :description "GIF decoder for LispWorks."
  :serial t
  :components ((:file "gif"))
  :depends-on ())
