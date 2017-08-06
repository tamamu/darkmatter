;;; user.lisp
;;-*- coding:utf-8 -*-

;;; Copyright (c) Eddie.
;;; Distributed under the terms of the MIT License.

(in-package :cl-user)
(defpackage darkmatter-user
  (:use :cl)
  (:nicknames :dm-user)
;  (:import-from :darkmatter.eval.rpc
;                :hook-eval-string-before
;                :hook-eval-string-after
;                :hook-eval-string-finalize)
  (:export :*plugin-handler*
           :*plugin-scripts*))
(in-package :darkmatter-user)

(defvar *plugin-handler*
  (make-hash-table :test #'equalp))

(defvar *plugin-scripts*
  (list))

(defun regist-plugin-handler (name handler)
  (setf (gethash name *plugin-handler*)
        handler))

(defun regist-plugin-script (name uri)
  (push `(("name" . ,name) ("path" . ,uri))
        *plugin-scripts*))

