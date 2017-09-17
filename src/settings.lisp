;;; settings.lisp
;;-*- coding:utf-8 -*-

;;; Copyright (c) Eddie.
;;; Distributed under the terms of the MIT License.

(in-package :cl-user)
(defpackage darkmatter.settings
  (:use :cl)
  (:export :*plugins*
           :load-settings))
(in-package :darkmatter.settings)

(defparameter *plugins*
  (list))

(defparameter *settings-file-path*
  (merge-pathnames ".darkmatter.conf" (user-homedir-pathname)))

(defun load-settings ()
  (if (probe-file *settings-file-path*)
      (load *settings-file-path*)))
#|
(let ((path (merge-pathnames ".darkmatter.conf" (user-homedir-pathname))))
  (if (probe-file path)
      (load path)))
|#

