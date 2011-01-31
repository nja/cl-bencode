;;; -*- Mode: Lisp -*-

(in-package #:cl-user)

(defpackage #:com.schobadoo.bencode
  (:nicknames #:bencode)
  (:use #:cl)
  (:import-from #:flexi-streams
                #:flexi-stream
                #:make-flexi-stream
                #:flexi-stream-external-format
                #:with-output-to-sequence
                #:with-input-from-sequence
                #:peek-byte)
  (:export #:encode
           #:decode
           #:get-dictionary
           #:dictionary-hide-binary))