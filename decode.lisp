;;; -*- Mode: LISP -*-
;;;
;;; Copyright (c) 2009-2012 Johan Andersson
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

(defmacro restart-case-loop (form &body clauses)
  `(loop (restart-case (return ,form)
	   ,@clauses)))

(defgeneric decode (input &key external-format)
  (:documentation "Decode a bencode object from a stream or sequence.
If input is a flexi-stream, its external-format will be used when
decoding strings.  If input is a string, all characters must have
char-codes that fit in an (unsigned-byte 8). Otherwise, the value of
the external-format parameter is used to create a flexi-stream for
decoding.  The default is UTF-8."))

(defmethod decode ((stream stream) &key (external-format :utf-8))
  (decode (make-flexi-stream stream :external-format external-format)))

(defmethod decode ((string string) &key (external-format :utf-8))
  (decode (map '(vector (unsigned-byte 8))
	       #'char-code string) :external-format external-format))

(defmethod decode ((sequence sequence) &key (external-format :utf-8))
  (restart-case-loop (with-input-from-sequence (stream sequence)
		       (decode (make-flexi-stream stream :external-format external-format)))
    (retry-sequence (new-external-format)
		    :report "Set external format and retry decoding the sequence from the beginning"
		    :interactive read-external-format
		    (setf external-format new-external-format))))

(defmethod decode ((stream flexi-stream) &key &allow-other-keys)
  (let ((c (code-char (peek-byte stream))))
    (case c
      (#\i (decode-integer stream))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
	 (decode-string stream))
      (#\l (decode-list stream))
      (#\d (decode-dictionary stream))
      (t (error 'invalid-value-type :octet c)))))

(define-condition unexpected-octet (error)
  ((expected-octet :initarg :expected-octet :reader expected-octet)
   (actual-octet :initarg :actual-octet :reader actual-octet)))

(define-condition invalid-value-type (error)
  ((octet :initarg :octet :reader octet)))

(defun must-read-char (stream char)
  (restart-case
      (let ((byte (read-byte stream)))
        (if (eql byte (char-code char))
            t
            (error 'unexpected-octet
		   :expected-octet (char-code char)
		   :actual-octet byte)))
    (continue () t)))

(defun char-integer-p (char)
  (case char
    ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) t)
    (t nil)))

(defun maybe-read-char (stream char)
  (if (eql (peek-byte stream nil t) (char-code char))
      (code-char (read-byte stream :eof-error-p t))
      nil))

(defun decode-integer (stream)
  (must-read-char stream #\i)
  (let* ((minus (maybe-read-char stream #\-))
         (integers (read-integers stream))
         (number (parse-integer integers)))
    (if (= number 0)
	(when (or minus (> (length integers) 1))
	  (restart-case (error "Zero must be i0e") (continue ())))
	(when (char= (elt integers 0) #\0)
	  (restart-case (error "Zero-padded integer") (continue ()))))
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

(defun must-read-octets (stream length)
  (let* ((array (make-array length :element-type '(unsigned-byte 8)))
	 (read (read-sequence array stream)))
    (if (= read length)
	array
	(restart-case (error "EOF before sting end")
	  (continue () (adjust-array array read))))))

(defun decode-string (stream)
  (with-accessors ((external-format flexi-stream-external-format))
      stream
    (let ((length (parse-integer (read-integers stream))))
      (must-read-char stream #\:)
      (let ((octets (must-read-octets stream length)))
	(restart-case-loop (octets-to-string octets :external-format external-format)
	  (use-binary ()
		      :report "Use undecoded binary vector"
		      (return octets))
	  (retry-string (new-external-format)
			:report "Set external format and continue decoding from the start of the string"
			:interactive read-external-format
			(setf external-format new-external-format)))))))

(defun decode-list (stream)
  (must-read-char stream #\l)
  (loop until (maybe-read-char stream #\e)
        collect (let ((*dictionary-keys* (cons :list *dictionary-keys*)))
		  (decode stream))))

(defun decode-binary-string (stream)
  (let ((length (parse-integer (read-integers stream))))
    (must-read-char stream #\:)
    (must-read-octets stream length)))

(defvar *dictionary-keys* nil)

(defun decode-dictionary (stream)
  (must-read-char stream #\d)
  (loop with list
	with previous-key
        until (maybe-read-char stream #\e)
        do (let ((key (decode-string stream)))
	     (when (and previous-key (not (string< previous-key key)))
	       (restart-case (error "Key ~S before key ~S in dict" previous-key key)
		 (continue ())))
             (let* ((*dictionary-keys* (cons key *dictionary-keys*))
                    (value (if (binary-dictionary-key-p *dictionary-keys*)
                               (decode-binary-string stream)
                               (decode stream))))
               (push value list)
               (push key list)
               (setf previous-key key)))
        finally (return (make-dictionary list))))
