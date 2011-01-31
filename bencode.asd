;;; -*- Mode: Lisp -*-

(defpackage #:bencode-system (:use #:asdf #:cl))
(in-package #:bencode-system)

(defsystem #:bencode
  :description "Bencode"
  :version "0.2"
  :author "Johan Andersson <johan@nforced.com>"
  :license "MIT"
  :components ((:file "package")
               (:file "dictionary" :depends-on ("package"))
               (:file "bencode" :depends-on ("package" "dictionary")))
  :depends-on (#:flexi-streams))
