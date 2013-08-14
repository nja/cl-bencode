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

(define-condition nonstring-dictionary-key (error)
  ((key :initarg :key :reader key)))

(defun make-dictionary (list)
  "Makes a dictionary from a plist or alist.  Keys must be strings."
  (let ((dictionary (make-hash-table :test 'equal)))
    (labels ((add-key-value (key value)
               (if (stringp key)
                   (setf (gethash key dictionary) value)
                   (restart-case (error 'nonstring-dictionary-key :key key)
                     (skip-key ())
                     (use-value (key)
		       :report "Specify string to use as key"
		       :interactive (lambda ()
		       		      (format t "Enter a key string: ")
		       		      (list (read)))
                       (add-key-value key value))))))
      (if (consp (car list))            ; alist
          (dolist (cons list dictionary)
            (destructuring-bind (key . value) cons
              (add-key-value key value)))
          (loop for (key value) on list by #'cddr ; plist
                do (add-key-value key value)))
      dictionary)))

(defparameter *binary-key-p* #'(lambda (x) (equal x '("pieces" "info")))
  "When decoding dictionary values, this function is passed a list,
where the first element is the key of the value. If the dictionary was
in turn a dictionary value, that key is the second element of the
list, and so on. Should a dictionary be a value in a bencoded list,
the corresponding element in the list will be the symbol :list.  When
the function return a true value, the dictionary value will be
binary. Otherwise it will be decoded as a string.

The default function in \*binary-key-p\* returns true for the
\"pieces\" value in the \"info\" dictionary. All other values are
decoded as strings.")

(defun get-dictionary (key dictionary)
  (gethash key dictionary))

(defun binary-dictionary-key-p (key)
  (when (functionp *binary-key-p*)
    (funcall *binary-key-p* key)))

(defun dictionary->alist (dictionary)
  "Returns an alist representation of the dictionary."
  (let ((alist))
    (labels ((add-key-value (key value)
               (if (stringp key)
                   (push (cons key value) alist)
                   (restart-case (error 'nonstring-dictionary-key :key key)
                     (skip-key ())
                     (use-value (key) :report "Specify string to use as key"
                       (add-key-value key value))))))
      (maphash #'add-key-value dictionary)
      (sort alist #'string< :key #'car))))
