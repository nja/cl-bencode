;;; -*- Mode: LISP -*-
;;;
;;; Copyright (c) 2009 Johan Andersson
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
      (cond ((consp (car list))         ; alist
             (dolist (cons list dictionary)
               (destructuring-bind (key . value) cons
                 (add-key-value key value))))
            (t                          ; plist
             (loop for (key value) on list by #'cddr
                   do (add-key-value key value))
             dictionary)))))

(defun dictionary-hide-binary (dictionary)
  (let ((copy (make-dictionary nil)))
    (maphash (lambda (key value)
               (unless (binary-dictionary-key-p key)
                 (setf (gethash key copy) value)))
             dictionary)
    copy))

(defparameter *binary-bdictionary-keys* (list "pieces"))

(defun get-dictionary (key dictionary)
  (gethash key dictionary))

(defun binary-dictionary-key-p (key)
  (find key *binary-bdictionary-keys* :test #'equal))

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
      (maphash #'add-key-value dictionary))
    (sort alist #'string< :key #'car)))