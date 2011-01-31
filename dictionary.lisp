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
                     (use-value (key) :report "Specify string to use as key"
                       (add-key-value key value))))))
      (if (consp (car list))            ; alist
          (dolist (cons list dictionary)
            (destructuring-bind (key . value) cons
              (add-key-value key value)))
          (loop for (key value) on list by #'cddr ; plist
                do (add-key-value key value)))
      dictionary)))

(defparameter *binary-dictionary-keys* '(("info" . "pieces"))
  "A list of strings and dotted lists of strings.  Strings denote
dictionary keys whose values will not be decoded, returning a byte
vector rather than a string.  Dotted lists are used to specify nested
dictionaries.  When decoding the value of a key matching the car of a
dotted list, the cdr will be passed on as a new binding of
*binary-dictionary-keys*.  Decoding a list will pass on the cdr of
lists with a car of nil.")

(defun get-dictionary (key dictionary)
  (gethash key dictionary))

(defun binary-dictionary-key-p (key)
  (find key *binary-dictionary-keys* :test 'equal))

(defun binary-sub-keys (key)
  (mapcar #'cdr (remove-if-not (lambda (x) (and (consp x)
                                                (equal key (car x))))
                               *binary-dictionary-keys*)))

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
