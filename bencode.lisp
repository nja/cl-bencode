;;; -*- Mode: LISP -*-
;;;
;;; Copyright (c) 2009-2010 Johan Andersson
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

(in-package #:com.schobadoo.bencode)

(defparameter +ascii+ (flex:make-external-format :ascii))

(defparameter +utf-8+ (flex:make-external-format :utf-8))

(defvar *external-format* nil)

(defun external-format (stream)
  (or *external-format*
      (flex:flexi-stream-external-format stream)))

(define-condition unexpected-octet (error)
  ((expected-octet :initarg :expected-octet :reader expected-octet)
   (actual-octet :initarg :actual-octet :reader actual-octet)))

(defun must-read-char (stream char)
  (restart-case
      (let ((byte (read-byte stream)))
        (if (eq byte (char-code char))
            t
            (error "Expected 0x~x got 0x~x" (char-code char) byte)))
    (continue () t)))

(defun string-to-octets (string &optional (external-format +ascii+))
  (flex:string-to-octets string :external-format external-format))

(defun octets-to-string (octets external-format)
  (flex:octets-to-string octets :external-format external-format))

(defgeneric encode (object stream-or-symbol)
  (:documentation "Encode object and write it to stream or, if
stream-or-symbol is a symbol, use an in-memory stream and return the
resulting sequence.  "))

(defmethod encode (object (external-format-keyword symbol))
  (let ((*external-format* (flex:make-external-format external-format-keyword)))
    (flex:with-output-to-sequence (stream)
      (encode object stream))))

(defmethod encode ((list list) (stream flexi-stream))
  (write-byte (char-code #\l) stream)
  (dolist (x list)
    (encode x stream))
  (write-byte (char-code #\e) stream))

(defmethod encode ((dictionary hash-table) (stream flexi-stream))
  (write-byte (char-code #\d) stream)
  (dolist (x (dictionary->alist dictionary))
    (destructuring-bind (k . v) x
      (encode k stream)
      (encode v stream)))
  (write-byte (char-code #\e) stream))

(defmethod encode ((string string) (stream flexi-stream))
  (let ((octets (string-to-octets string (external-format stream))))
    (write-sequence (string-to-octets (format nil "~a:" (length octets))) stream)
    (write-sequence octets stream)))

(defmethod encode ((integer integer) (stream flexi-stream))
  (write-sequence (string-to-octets (format nil "i~ae" integer)) stream))

(defmethod encode ((sequence array) (stream flexi-stream))
  (write-sequence (string-to-octets (format nil "~a:" (length sequence))) stream)
  (write-sequence sequence stream))

(defgeneric decode (stream)
  (:documentation "Decode a bencode object from stream.  If stream is a flexi-stream,
its external-format will be used for decoding strings.  If stream is
an ordinary stream, a flexi-stream with an external-format of :utf-8
will be created and used."))

(defmethod decode ((stream stream))
  (decode (flex:make-flexi-stream stream :external-format +utf-8+)))

(defmethod decode ((stream flexi-stream))
  (let ((c (code-char (flex:peek-byte stream))))
    (ccase c
      (#\i (decode-integer stream))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
         (decode-string stream))
      (#\l (decode-list stream))
      (#\d (decode-dictionary stream)))))

(defun char-integer-p (char)
  (case char
    ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) t)
    (t nil)))

(defun char->integer (char)
  (when (char-integer-p char)
    (- (char-int char) (char-int #\0))))

(defun maybe-read-char (stream char)
  (if (eq (flex:peek-byte stream) (char-code char))
      (code-char (read-byte stream))
      nil))

(defun decode-integer (stream)
  (must-read-char stream #\i)
  (let* ((minus (maybe-read-char stream #\-))
         (integers (read-integers stream))
         (number (parse-integer integers)))
    (when (and (= number 0) (or minus (> (length integers) 1)))
      (restart-case (error "Zero must be i0e")
        (continue () 0)))
    (must-read-char stream #\e)
    (if minus
        (- number)
        number)))

(defun read-integers (stream)
  (with-output-to-string (string)
    (loop for octet = (flex:peek-byte stream)
          while (char-integer-p (code-char octet))
          do (write-char (code-char (read-byte stream)) string))))

(defun decode-string (stream)
  (let* ((length (parse-integer (read-integers stream)))
         (octets (make-array length :element-type '(unsigned-byte 8))))
    (must-read-char stream #\:)
    (read-sequence octets stream)
    (loop    ; Loop to allow restarting with several external formats 
      (restart-case
          (return ; Return to end loop when decoded without raising a condition
            (octets-to-string octets (external-format stream)))
        (use-binary ()
          :report "Use undecoded binary vector"
          octets)
        (set-external-format (external-format)
          :report "Set external format"
          :interactive (lambda ()
                         (format t "Enter an external format keyword: ")
                         (multiple-value-list (eval (read))))
          (let ((external-format (flex:make-external-format external-format)))
            (if *external-format*
                (setf *external-format* external-format)
                (setf (flex:flexi-stream-external-format stream) external-format))))))))

(defun decode-list (stream)
  (must-read-char stream #\l)
  (loop with list
        until (maybe-read-char stream #\e)
        do (push (decode stream) list)
        finally (return (nreverse list))))

(defun decode-binary-string (stream)
  (let* ((length (parse-integer (read-integers stream)))
         (octets (make-array length :element-type '(unsigned-byte 8))))
    (must-read-char stream #\:)
    (read-sequence octets stream)
    octets))

(defun decode-dictionary (stream)
  (must-read-char stream #\d)
  (loop with list
        until (maybe-read-char stream #\e)
        do (let* ((key (decode-string stream))
                  (value (if (binary-dictionary-key-p key)
                             (decode-binary-string stream)
                             (decode stream))))
             (push value list)
             (push key list))
        finally (return (make-dictionary list))))
