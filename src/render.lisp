;;; render.lisp
;;-*- coding:utf-8 -*-

;;; Copyright (c) Eddie.
;;; Distributed under the terms of the MIT License.

(in-package :cl-user)
(defpackage darkmatter.web.render
  (:use :cl)
  (:export :notfound
           :render-index
           :render-files
           :render-notebook))
(in-package :darkmatter.web.render)

(defun notfound (env)
  )

(defun render-index (env)
  )

(defun render-files (env path)
  )

(defun render-notebook (env path)
  )
