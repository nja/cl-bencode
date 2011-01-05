;;; -*- Mode: Lisp -*-

(in-package #:cl-user)

(defpackage #:com.schobadoo.bencode
  (:nicknames #:bencode)
  (:use #:cl)
  (:import-from #:flexi-streams #:flexi-stream)
  (:export #:encode
           #:decode
           #:get-dictionary
           #:dictionary-hide-binary))