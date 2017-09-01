;;; client/user.lisp
;;-*- coding:utf-8 -*-

;;; Copyright (c) Eddie.
;;; Distributed under the terms of the MIT License.

(in-package :cl-user)
(defpackage darkmatter-client-user
  (:use :cl)
  (:nicknames :dmc-user)
  (:import-from :darkmatter.utils
                :%log)
  (:import-from :darkmatter.settings
                :*plugins*)
  (:export :*plugin-handler*
           :*plugin-scripts*
           :*plugin-methods*
           :plugin-root-uri
           :regist-plugin-handler
           :regist-plugin-script
           :regist-plugin-method
           :load-web-plugins))
(in-package :darkmatter-client-user)


(defvar *plugin-handler*
  (make-hash-table :test #'equalp))

(defvar *plugin-scripts*
  (list))

(defvar *plugin-methods*
  (make-hash-table :test #'equalp))

(defun %plugin-name ()
  (let ((plugin.web (string-downcase (package-name *package*))))
    (subseq plugin.web 0 (- (length plugin.web) 4))))

(defun %plugin-entry (hash-table)
  (let ((entry (gethash (%plugin-name) hash-table nil)))
    (if entry
        entry
        (setf (gethash (%plugin-name) hash-table)
              (make-hash-table :test #'equalp)))))

(defun plugin-root-uri ()
  (format nil "/plugin/~A" (%plugin-name)))

(defun regist-plugin-handler (handler)
  (%log (format nil "Set ~A handler" (%plugin-name)) :INIT)
  (setf (gethash (%plugin-name) *plugin-handler*) handler))

(defun regist-plugin-script (path)
  (push `(("name" . ,(%plugin-name)) ("path" . ,path))
        *plugin-scripts*))

(defun regist-plugin-method (method-name function)
  (let ((entry (%plugin-entry *plugin-methods*)))
    (setf (gethash method-name entry)
          function)))

(defun load-web-plugins ()
  (mapcar (lambda (plugin)
            (require (format nil "~A-web" plugin)))
          *plugins*))

