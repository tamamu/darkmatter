;;; server/user.lisp
;;-*- coding:utf-8 -*-

;;; Copyright (c) Eddie.
;;; Distributed under the terms of the MIT License.

(in-package :cl-user)
(defpackage darkmatter-user
  (:use :cl)
  (:nicknames :dm-user)
  (:import-from :darkmatter.settings
                :*plugins*)
  (:export :*debug*
           :*eval-string-before-hooks*
           :*eval-string-after-hooks*
           :*eval-string-finalize-hooks*
           :hook-eval-string-before
           :hook-eval-string-after
           :hook-eval-string-finalize
           :load-eval-plugins))
(in-package :darkmatter-user)

(defvar *debug* nil)

(defun %identity (first &rest rest)
  first)

(defvar *eval-string-before-hooks*
  (list #'(lambda (sexp optional) (values sexp optional))))

(defvar *eval-string-after-hooks*
  (list #'(lambda (return-value optional) (values return-value optional))))

(defvar *eval-string-finalize-hooks*
  (list))

(defun hook-eval-string-before (hook)
  (format t "[Hook] ~A before eval string~%" hook)
  (push hook *eval-string-before-hooks*))

(defun hook-eval-string-after (hook)
  (format t "[Hook] ~A after eval string~%" hook)
  (push hook *eval-string-after-hooks*))

(defun hook-eval-string-finalize (hook)
  (format t "[Hook] ~A finally eval string~%" hook)
  (push hook *eval-string-finalize-hooks*))

(defun load-eval-plugins (&optional plugins)
  (format t "[Load] ~A~%" (or plugins *plugins*))
  (force-output)
  (mapcar (lambda (plugin)
            (handler-case
              (require (format nil "~A/darkmatter" plugin))
              (error (c) (format nil "Failed to load ~A~%" plugin))))
          (or plugins *plugins*)))
