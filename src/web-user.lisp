;;; web-user.lisp
;;-*- coding:utf-8 -*-

;;; Copyright (c) Eddie.
;;; Distributed under the terms of the MIT License.

(in-package :cl-user)
(defpackage darkmatter.web.user
  (:use :cl)
  (:nicknames :dm-web-user)
  (:export :*plugin-handler*
           :*plugin-scripts*
           :*plugin-methods*
           :regist-plugin-handler
           :regist-plugin-script
           :regist-plugin-method))
(in-package :darkmatter.web.user)

(defvar *plugin-handler*
  (make-hash-table :test #'equalp))

(defvar *plugin-scripts*
  (list))

(defvar *plugin-methods*
  (make-hash-table :test #'equalp))

(defun %plugin-entry (hash-table)
  (let ((entry (gethash (string-downcase (package-name *package*)) hash-table nil)))
    (if entry
        entry
        (setf (gethash (string-downcase (package-name *package*)) hash-table)
              (make-hash-table :test #'equalp)))))

(defun regist-plugin-handler (handler)
  (format t "[Plugin] Set ~A handler~%" (string-downcase (package-name *package*)))
  (setf (gethash (string-downcase (package-name *package*)) *plugin-handler*) handler))

(defun regist-plugin-script (path)
  (push `(("name" . ,(string-downcase (package-name *package*))) ("path" . ,path))
        *plugin-scripts*))

(defun regist-plugin-method (method-name function)
  (let ((entry (%plugin-entry *plugin-methods*)))
    (setf (gethash method-name entry)
          function)))
