;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COM.SCHOBADOO.BENCODE -*-

(in-package :com.schobadoo.bencode)

(defun maybe-read-char (stream chars)
  (let ((chars (if (characterp chars)
                   (list chars)
                   chars)))
    (if (find (peek-char nil stream) chars)
        (read-char stream)
        nil)))

(defun must-read-char (stream char)
  (if (char= (read-char stream) char)
      t
      (error "Didn't read ~a" char)))

(defparameter *encode-chunk-size* 64)

(defun encode (x &optional (array (make-array *encode-chunk-size*
                                              :element-type '(unsigned-byte 8)
                                              :adjustable t :fill-pointer 0)))
  (etypecase x
    (bdictionary (encode-dictionary x array))
    (cons (encode-list x array))
    (integer (encode-integer x array))
    (string (encode-string x array))
    (symbol (encode-string (format nil "SYMBOL:~a" x) array))))

(defun encode-list (list array)
  (vector-push-extend-string "l" array)
  (dolist (x list)
    (encode x array))
  (vector-push-extend-string "e" array))

(defun encode-dictionary (dictionary array)
  (vector-push-extend-string "d" array)
  (dolist (x (bdictionary->alist dictionary))
    (destructuring-bind (k . v) x
      (encode k array)
      (encode v array)))
  (vector-push-extend-string "e" array))

(defun encode-string (string array)
  (encode-string-length (length string) array)
  (vector-push-extend-string string array))

(defun encode-string-length (length array)
  (vector-push-extend-string (format nil "~a:" length) array))

(defun vector-push-extend-string (string vector)
  (loop for c across string
       do (vector-push-extend (char-code c) vector))
  vector)

(defun encode-integer (integer array)
  (vector-push-extend-string (format nil "i~ae" integer) array))

(defun decode (stream)
  (let ((c (peek-char nil stream nil)))
    (if (null c)
        nil
        (ccase c
          (#\i (decode-integer stream))
          ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
           (decode-string stream))
          (#\l (decode-list stream))
          (#\d (decode-dictionary stream))))))

(defun char-integer-p (char)
  (case char
    ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) t)
    (t nil)))

(defun char->integer (char)
  (when (char-integer-p char)
    (- (char-int char) (char-int #\0))))

(defun decode-integer (stream &optional (end-char #\e))
  (must-read-char stream #\i)
  (let* ((minus (maybe-read-char stream #\-))
         (integers (read-integers stream))
         (number (parse-integer integers)))
    (when (and (= number 0) (or minus (> (length integers) 1)))
      (error "Zero must be i0e"))
    (prog1 (if minus
               (- number)
               number)
      (must-read-char stream end-char))))

(defun read-integers (stream)
  (with-output-to-string (string)
    (loop for peek = (peek-char nil stream)
       while (char-integer-p peek)
       do (write-char (read-char stream) string))))

(defun decode-string (stream)
  (let* ((length (parse-integer (read-integers stream)))
         (string (make-string length)))
    (must-read-char stream #\:)
    (loop for i below length
       do (setf (elt string i) (read-char stream)))
    string))

(defun decode-list (stream &optional (begin-char #\l))
  (must-read-char stream begin-char)
  (let ((list nil))
    (loop for c = (read-char stream)
       until (char= c #\e)
       do (progn
            (unread-char c stream)
            (push (decode stream) list)))
    (nreverse list)))

(defun decode-dictionary (stream)
  (make-bdictionary (decode-list stream #\d)))

