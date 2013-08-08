;;; -*- Mode: Lisp -*-

(defpackage #:bencode-system (:use #:asdf #:cl))
(in-package #:bencode-system)

(defsystem #:bencode
  :description "Bencode"
  :version "3.0.0"
  :author "Johan Andersson <nilsjohanandersson@gmail.com>"
  :license "MIT"
  :components ((:file "package")
               (:file "dictionary" :depends-on ("package"))
               (:file "encode" :depends-on ("package" "dictionary"))
	       (:file "decode" :depends-on ("package" "dictionary")))
  :depends-on (#:flexi-streams)
  :in-order-to ((test-op (test-op #:bencode-test))))

(defsystem #:bencode-test
  :serial t
  :depends-on (#:bencode #:hu.dwim.stefil)
  :components ((:file "test")))

(defmethod perform ((o test-op) (c (eql (find-system :bencode-test))))
  (funcall (intern (symbol-name :test-all) (find-package :bencode-test))))
