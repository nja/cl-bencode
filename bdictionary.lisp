;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COM.SCHOBADOO.BENCODE -*-

(in-package :com.schobadoo.bencode)

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