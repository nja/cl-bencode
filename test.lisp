;;; -*- Mode: Lisp -*-

(defpackage #:bencode-test
  (:use #:cl #:bencode #:hu.dwim.stefil)
  (:export #:test-all))

(in-package #:bencode-test)

(defsuite* test-all)

;;; Integers

(deftest decode-integer-test ()
  (dolist (case '(("i0e" 0)
		  ("i-1e" -1)
		  ("i23e" 23)))
    (destructuring-bind (input expected) case
      (is (= expected (decode input))))))

(deftest integer-rountrip-test ()
  (dolist (integer (list 0 1 -2 most-positive-fixnum most-negative-fixnum
			 (1+ most-positive-fixnum)
			 (1- most-negative-fixnum)))
    (is (= integer (decode (encode integer nil))))))

(deftest encode-unknown-type-error-test ()
  (signals error (encode 'symbol nil)))

(deftest decode-bad-integer-test ()
  (dolist (input '("i-0e" "i00e" "i01e" "i-02e" "i3.0e" "i0"))
    (signals error (decode input))))

;;; Strings

(defparameter *non-ascii-string* "räksmörgås½§")

(defparameter *encodings* '(:iso-8859-1 :utf-8 :utf-16 :utf-32)
  "Encodings ordered by the length of encoded latin code points.")

(deftest external-format-roundtrip-test ()
  (dolist (encoding *encodings*)
    (let ((string *non-ascii-string*))
      (is (string= string (decode (encode *non-ascii-string* nil
					  :external-format encoding)
				  :external-format encoding))))))

(deftest external-format-length-test ()
  (let ((encoded-lengths (mapcar #'(lambda (enc)
				     (length (encode *non-ascii-string* nil
						     :external-format enc)))
				 *encodings*)))
    (is (apply #'<= encoded-lengths))))

(deftest decode-string-specializer-test ()
  (is (string= "foo" (decode "3:foo"))))

(deftest wrong-string-length-test ()
  (signals error (decode "3:ab")))

;;; Lists

(deftest list-roundtrip-test ()
  (dolist (list '(nil (1) (("a" 2)) ("c" (("d") 5 ()))))
    (is (equalp list (decode (encode list nil))))))

(deftest list-not-closed-test ()
  (signals error (decode "li0e")))

;;; Dictionaries

(deftest dict-roundtrip-test ()
  (dolist (dict (mapcar #'bencode::make-dictionary
			'(()
			  ("foo" "bar")
			  ("aa" 12 "ab" 34))))
    (is (equalp dict (decode (encode dict nil))))))

(deftest binary-dictionary-keys-test ()
  (let* ((bencode:*binary-dictionary-keys* '(("info" . "binary")))
	 (dict (decode "d4:infod6:binary3:abc6:string3:cdeee")))
    (is (not (typep (gethash "binary" (gethash "info" dict))
		    'string)))
    (is (typep (gethash "string" (gethash "info" dict))
	       'string))))

(deftest bad-dictionary-test ()
  (dolist (dict '("d3:fooe" 		; Missing value
		  "di0e0:e"		; Non-string key
		  "d1:a0:"		; Not closed with #\e
		  "d1:z0:1:a0:e"	; Keys not sorted
		  ))
    (signals error (decode dict))))