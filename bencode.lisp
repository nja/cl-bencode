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
  (cond ((bdictionary-p x) (encode-dictionary x array)) ; Must preceed the list case
        ((listp x) (encode-list x array))
        ((integerp x) (encode-integer x array))
        ((stringp x) (encode-string x array))
        ((symbolp x) (encode-string (format nil "SYMBOL:~a" x) array))
        (t (error "Can only encode bdictionaries, lists, strings and integers"))))

(defun encode-list (list array)
  (vector-push-extend-string "l" array)
  (dolist (x list)
    (encode x array))
  (vector-push-extend-string "e" array))

(defun encode-dictionary (dictionary array)
  (vector-push-extend-string "d" array)
  (dolist (x (rest dictionary))
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

(defun make-bdictionary (list)
  (cons 'bdictionary
        (sort (loop for (k v) on list by #'cddr
                 collect (cons k v))
              #'string<
              :key #'car)))

(defun bdictionary-p (x)
  (when (listp x)
    (eq (first x) 'bdictionary)))

(defun decode-dictionary (stream)
  (make-bdictionary (decode-list stream #\d)))

(defun bdictionary-hide-binary (x)
  (cond ((bdictionary-p x)
         (cons 'bdictionary
          (mapcar (lambda (x)
                    (destructuring-bind (k . v) x
                     (if (binary-bdictionary-key-p k)
                         (cons k (list 'binary (length v)))
                         (cons k (bdictionary-hide-binary v)))))
                  (rest x))))
        ((listp x) (mapcar #'bdictionary-hide-binary x))
        (t x)))

(defparameter *binary-bdictionary-keys* (list "pieces"))

(defun binary-bdictionary-key-p (key)
  (find key *binary-bdictionary-keys* :test #'equal))

(defun get-bdictionary (key bdictionary)
  (if (bdictionary-p bdictionary)
      (cdr (assoc key (rest bdictionary) :test #'equal))
      (error "Not a bdictionary")))

(defun bytes-to-string (bytes)
  (loop with string = (make-string (length bytes))
       for i from 0
       for b across bytes
       do (setf (elt string i) (code-char b))
       finally (return string)))

