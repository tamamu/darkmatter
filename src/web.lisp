;;; web.lisp
;;-*- coding:utf-8 -*-

;;; Copyright (c) Eddie.
;;; Distributed under the terms of the MIT License.

(in-package :cl-user)
(defpackage darkmatter.web
  (:use :cl)
  (:import-from :darkmatter.web.handle
                :->get
                :->put)
  (:import-from :lack.builder
                :builder)
  (:import-from :alexandria
                :starts-with-subseq))
(in-package :darkmatter.web)

(defvar *root-directory*
  (asdf:system-relative-pathname "darkmatter.web" #P"app.lisp"))

(defparameter *web*
  (lambda (env)
    (case (getf env :request-method)
      (:GET (->get env))
      (:PUT (->put env))))
  "Handling HTTP methods")

(setf *web*
      (builder
        (:static :path (lambda (path)
                         (if (starts-with-subseq "/static/" path)
                             path
                             nil))
         :root *root-directory*)
        *web*))
