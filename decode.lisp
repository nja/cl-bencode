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

(defgeneric decode (stream-or-sequence &key external-format)
  (:documentation "Decode a bencode object from a stream or sequence.
If stream is a flexi-stream, its external-format will be used when
decoding strings.  Otherwise, the value of the external-format
parameter is used to create a flexi-stream for decoding.  The default
is UTF-8."))

(defmethod decode ((stream stream) &key (external-format :utf-8))
  (decode (make-flexi-stream stream :external-format external-format)))

(defmethod decode ((sequence sequence) &key (external-format :utf-8))
  (with-input-from-sequence (stream sequence)
    (decode (make-flexi-stream stream :external-format external-format))))

(defmethod decode ((stream flexi-stream) &key &allow-other-keys)
  (let ((c (code-char (peek-byte stream))))
    (ccase c
      (#\i (decode-integer stream))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
         (decode-string stream))
      (#\l (decode-list stream))
      (#\d (decode-dictionary stream)))))

(define-condition unexpected-octet (error)
  ((expected-octet :initarg :expected-octet :reader expected-octet)
   (actual-octet :initarg :actual-octet :reader actual-octet)))

(defun must-read-char (stream char)
  (restart-case
      (let ((byte (read-byte stream)))
        (if (eql byte (char-code char))
            t
            (error "Expected 0x~x got 0x~x" (char-code char) byte)))
    (continue () t)))

(defun char-integer-p (char)
  (case char
    ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) t)
    (t nil)))

(defun maybe-read-char (stream char)
  (if (eql (peek-byte stream) (char-code char))
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
    (loop for octet = (peek-byte stream)
          while (char-integer-p (code-char octet))
          do (write-char (code-char (read-byte stream)) string))))

(defun read-external-format ()
  (format t "Enter a flexi-stream external format: ")
  (multiple-value-list (eval (read))))

(defun decode-string (stream)
  (with-accessors ((external-format flexi-stream-external-format))
      stream
    (let* ((length (parse-integer (read-integers stream)))
	   (octets (make-array length :element-type '(unsigned-byte 8))))
      (must-read-char stream #\:)
      (read-sequence octets stream)
      (loop  ; Loop to allow restarting with several external formats 
	(restart-case
	    (return ; Return to end loop when decoded without raising a condition
	      (octets-to-string octets :external-format external-format))
	  (use-binary ()
	    :report "Use undecoded binary vector"
	    (return octets))
	  (set-external-format (new-external-format)
	    :report "Set external format"
	    :interactive (lambda ()
			   (format t "Enter a flexi-stream external format: ")
			   (multiple-value-list (eval (read))))
	    (setf external-format new-external-format)))))))

(defun decode-list (stream)
  (must-read-char stream #\l)
  (loop until (maybe-read-char stream #\e)
        collect (let ((*binary-dictionary-keys* (binary-sub-keys nil)))
		  (decode stream))))

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
                             (let ((*binary-dictionary-keys* (binary-sub-keys key)))
                               (decode stream)))))
             (push value list)
             (push key list))
        finally (return (make-dictionary list))))
