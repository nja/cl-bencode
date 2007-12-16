(defpackage :com.schobadoo.bencode-system (:use :asdf :cl))
(in-package :com.schobadoo.bencode-system)

(defsystem "bencode"
  :description "Bencode"
  :version "0.1"
  :author "Johan Andersson <johan@nforced.com>"
  :components ((:file "bencode-package")
               (:file "bencode" :depends-on ("bencode-package")))
  :depends-on (:trivial-utf-8))