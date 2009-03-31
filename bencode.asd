;;; -*- Mode: Lisp -*-

(defpackage #:com.schobadoo.bencode-system (:use #:asdf #:cl))
(in-package #:com.schobadoo.bencode-system)

(defsystem #:bencode
  :description "Bencode"
  :version "0.1"
  :author "Johan Andersson <johan@nforced.com>"
  :license "MIT"
  :components ((:file "bencode-package")
               (:file "dictionary" :depends-on ("bencode-package"))
               (:file "bencode" :depends-on ("bencode-package" "dictionary")))
  :depends-on (#:flexi-streams))
