;;; rpc.lisp
;;-*- coding:utf-8 -*-

;;; Copyright (c) Eddie.
;;; Distributed under the terms of the MIT License.

(in-package :cl-user)
(defpackage darkmatter.eval.rpc
  (:use :cl)
  (:export :+rpcdef-list+))
(in-package :darkmatter.eval.rpc)

(defvar +rpcdef-list+
  (list
    '("darkmatter/initialize" . #'initialize)
    '("darkmatter/eval" . #'eval-string)
    '("darkmatter/initialize-package" . #'initialize-package)
    '("darkmatter/get-share-object" . #'get-share-object)))


(defvar *default-package-definition*
  (list :use '(:cl)))


(defun initialize (|processId| |rootUri| |initializeOptions| |trace|)
  "Initialize darkmatter evaluation server.
   processId is the identifier for the instance of this program.

   * No response"
  (let ((plugins (getf |initializeOptions| |plugins|)))
    (when plugins
      (mapcar #'load plugins)))

  (let ((default-package (getf |initializeOptions| |defaultPackage|)))
    (when default-package
      (setf (getf *default-package-definition* :use)
            default-package)))

  (when |trace|
    (setf *trace-style* |trace|)))

(defun eval-string ()
  "Evaluate the string from the editor.

   * Response result"
  )

(defun initialize-package ()
  "Initialize the package.

   * No response"
  )

(defun get-share-object ()
  "Get a share object of the asynchronous task of the id.

   * Response object"
  )
