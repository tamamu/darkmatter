;;; client.lisp
;;-*- coding:utf-8 -*-

;;; Copyright (c) Eddie.
;;; Distributed under the terms of the MIT License.

(in-package :cl-user)
(defpackage darkmatter.client
  (:use :cl)
  (:import-from :darkmatter.client.handler
                :kill-all-process
                :->get
                :->put)
  (:import-from :clack
                :clackup)
  (:import-from :lack.builder
                :builder)
  (:import-from :alexandria
                :starts-with-subseq)
  (:export :start
           :stop))
(in-package :darkmatter.client)

(defvar *handler* nil)

(defvar *appfile-path*
  (asdf:system-relative-pathname "darkmatter-notebook" #P"app.lisp"))

(defvar *root-directory*
  (asdf:system-relative-pathname "darkmatter-notebook" ""))

(defparameter *web*
  (lambda (env)
    (case (getf env :request-method)
      (:GET (->get env))
      (:PUT (->put env))
      (t (format t "[Warn] Unknown HTTP method ~A has come~%"
                 (getf env :request-method)))))
  "Handling HTTP methods")

(setf *web*
      (builder
        (:static :path (lambda (path)
                         (if (starts-with-subseq "/static/" path)
                             path
                             nil))
         :root *root-directory*)
        *web*))


(defun start (&rest args &key server port &allow-other-keys)
  (declare (ignore server port))
  (when *handler*
    (restart-case (error "Darkmatter is already running.")
      (restart-darkmatter ()
        :report "Restart Darkmatter"
        (stop))))
  (setf *handler*
        (apply #'clackup *appfile-path* args)))

(defun stop ()
  (prog1
    (clack:stop *handler*)
    (setf *handler* nil)))

