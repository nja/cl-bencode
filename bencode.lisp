;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COM.SCHOBADOO.BENCODE -*-

(in-package :com.schobadoo.bencode)

(defun must-read-char (stream char)
  (if (eq (read-byte stream) (char-code char))
      t
      (error "Didn't read ~a" char)))

(defparameter *external-format* (flex:make-external-format :utf-8))

(defun string-to-octets (string)
  (flex:string-to-octets string :external-format *external-format*))

(defun octets-to-string (octets)
  (flex:octets-to-string octets :external-format *external-format*))

(defgeneric encode (object stream)
  (:documentation "Encode object and write it to stream.  If stream is
nil, an in-memory stream will be used and the resulting sequence
returned."))

(defmethod encode (object (stream (eql nil)))
  (flex:with-output-to-sequence (stream)
    (encode object stream)))

(defmethod encode ((list list) (stream stream))
  (write-byte (char-code #\l) stream)
  (dolist (x list)
    (encode x stream))
  (write-byte (char-code #\e) stream))

(defmethod encode ((dictionary bdictionary) (stream stream))
  (write-byte (char-code #\d) stream)
  (dolist (x (bdictionary->alist dictionary))
    (destructuring-bind (k . v) x
      (encode k stream)
      (encode v stream)))
  (write-byte (char-code #\e) stream))

(defmethod encode ((string string) (stream stream))
  (let ((octets (string-to-octets string)))
    (write-sequence (string-to-octets (format nil "~a:" (length octets))) stream)
    (write-sequence octets stream)))

(defmethod encode ((integer integer) (stream stream))
  (write-sequence (string-to-octets (format nil "i~ae" integer)) stream))

(defmethod encode ((sequence array) (stream stream))
  (write-sequence (string-to-octets (format nil "~a:" (length sequence))) stream)
  (write-sequence sequence stream))

(defun decode (stream &key (binary nil))
  (let ((c (code-char (flex:peek-byte stream))))
    (ccase c
      (#\i (decode-integer stream))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
         (if binary
             (decode-binary-string stream)
             (decode-string stream)))
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
      (error "Zero must be i0e"))
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
    (octets-to-string octets)))

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
                  (value (decode stream :binary (binary-bdictionary-key-p key))))
             (push value list)
             (push key list))
        finally (return (make-bdictionary list))))

