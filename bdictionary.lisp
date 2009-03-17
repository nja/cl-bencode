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

(defclass bdictionary ()
  ((hashtable :initform (make-hash-table :test 'equal))))

(defun make-bdictionary (list)
  "Makes a bdictionary instance from a plist.  Keys must be strings."
  (let ((bdictionary (make-instance 'bdictionary)))
    (with-slots (hashtable) bdictionary
      (loop for (key value) on list by #'cddr
         do (assert (stringp key))
           (setf (gethash key hashtable) value)))
    bdictionary))

(defun bdictionary-hide-binary (bdictionary)
  (let ((copy (make-instance 'bdictionary)))
    (with-slots ((old-hash hashtable)) bdictionary
      (with-slots ((new-hash hashtable)) copy
        (maphash (lambda (key value)
                   (unless (binary-bdictionary-key-p key)
                     (setf (gethash key new-hash) value)))
                 old-hash)))
    copy))

(defparameter *binary-bdictionary-keys* (list "pieces"))

(defun binary-bdictionary-key-p (key)
  (find key *binary-bdictionary-keys* :test #'equal))

(defun get-bdictionary (key bdictionary)
  (gethash key (slot-value bdictionary 'hashtable)))

(defun bdictionary->alist (bdictionary)
  "Returns an alist representation of the bdictionary."
  (let ((alist))
   (maphash (lambda (key value)
              (push (cons key value) alist))
            (slot-value bdictionary 'hashtable))
   (sort alist #'string< :key #'car)))