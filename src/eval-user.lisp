;;; eval-user.lisp
;;-*- coding:utf-8 -*-

;;; Copyright (c) Eddie.
;;; Distributed under the terms of the MIT License.

(in-package :cl-user)
(defpackage darkmatter.eval.user
  (:use :cl)
  (:nicknames :dm-eval-user)
  (:export :*eval-string-before-hooks*
           :*eval-string-after-hooks*
           :*eval-string-finalize-hooks*
           :hook-eval-string-before
           :hook-eval-string-after
           :hook-eval-string-finalize))
(in-package :darkmatter.eval.user)

(defvar *eval-string-before-hooks*
  (list #'identity))

(defvar *eval-string-after-hooks*
  (list #'identity))

(defvar *eval-string-finalize-hooks*
  (list))

(defun hook-eval-string-before (hook)
  (push hook *eval-string-before-hooks*))

(defun hook-eval-string-after (hook)
  (push hook *eval-string-after-hooks*))

(defun hook-eval-string-finalize (hook)
  (push hook *eval-string-finalize-hooks*))
