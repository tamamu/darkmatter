;;; settings.lisp
;;-*- coding:utf-8 -*-

;;; Copyright (c) Eddie.
;;; Distributed under the terms of the MIT License.

(in-package :cl-user)
(defpackage darkmatter.settings
  (:use :cl :dm-eval-user :dm-web-user)
  (:export :*plugins*
           :load-web-plugins
           :load-eval-plugins))
(in-package :darkmatter.settings)

(defparameter *plugins*
  (list))

(let ((path (merge-pathnames ".darkmatter.conf" (user-homedir-pathname))))
  (if (probe-file path)
      (load path)))

(mapcar #'require *plugins*)

(defun load-web-plugins ()
  (mapcar (lambda (system)
            (load (format nil "~A.web" system)))
          *plugins*))

(defun load-eval-plugins ()
  (mapcar (lambda (system)
            (load (format nil "~A.eval" system)))
          *plugins*))
