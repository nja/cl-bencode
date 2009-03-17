;;; -*- Mode: Lisp -*-

(in-package #:cl-user)

(defpackage #:com.schobadoo.bencode
  (:nicknames #:bencode)
  (:use #:cl)
  (:export #:encode
           #:decode
           #:bdictionary
           #:get-bdictionary
           #:bdictionary-hide-binary))