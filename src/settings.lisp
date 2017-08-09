;;; settings.lisp
;;-*- coding:utf-8 -*-

;;; Copyright (c) Eddie.
;;; Distributed under the terms of the MIT License.

(in-package :cl-user)
(defpackage darkmatter.settings
  (:use :cl)
  (:export :*plugins*))
(in-package :darkmatter.settings)

(defparameter *plugins*
  (list))

(let ((path (merge-pathnames ".darkmatter.conf" (user-homedir-pathname))))
  (if (probe-file path)
      (load path)))

