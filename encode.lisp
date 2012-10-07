;;; -*- Mode: LISP -*-
;;;
;;; Copyright (c) 2009-2011 Johan Andersson
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;; 
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.

(in-package #:bencode)

(defgeneric encode (object stream &key external-format)
  (:documentation "Encode object and write it to stream or, if stream
is nil, use an in-memory stream and return the resulting sequence.
The external-format is used when encoding strings.  UTF-8 is the
default."))

(defmethod encode (object (stream stream) &key (external-format :utf-8))
  (if (typep stream 'flexi-stream)
      (error "No applicable encode method for ~S" object)
      (encode object (make-flexi-stream stream :external-format external-format))))

(defmethod encode (object (stream (eql nil)) &key (external-format :utf-8))
  (with-output-to-sequence (stream)
    (encode object (make-flexi-stream stream :external-format external-format))))

(defmethod encode ((list list) (stream flexi-stream) &key &allow-other-keys)
  (write-byte (char-code #\l) stream)
  (dolist (x list)
    (encode x stream))
  (write-byte (char-code #\e) stream))

(defmethod encode ((dictionary hash-table) (stream flexi-stream) &key &allow-other-keys)
  (write-byte (char-code #\d) stream)
  (dolist (x (dictionary->alist dictionary))
    (destructuring-bind (k . v) x
      (encode k stream)
      (encode v stream)))
  (write-byte (char-code #\e) stream))

(defmethod encode ((string string) (stream flexi-stream) &key &allow-other-keys)
  (with-accessors ((external-format flexi-stream-external-format))
      stream
    (let ((length (octet-length string :external-format external-format)))
      (write-sequence (string-header length) stream)
      (write-sequence string stream))))

(defmethod encode ((integer integer) (stream flexi-stream) &key &allow-other-keys)
  (write-sequence (render-integer integer) stream))

(defmethod encode ((sequence array) (stream flexi-stream) &key &allow-other-keys)
  (write-sequence (string-header (length sequence)) stream)
  (write-sequence sequence stream))

(let ((ascii (flex:make-external-format :ascii)))
  (defun string-header (length)
    (string-to-octets (format nil "~a:" length) :external-format ascii))

  (defun render-integer (integer)
    (string-to-octets (format nil "i~ae" integer) :external-format ascii)))
