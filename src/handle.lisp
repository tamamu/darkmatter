;;; handle.lisp
;;-*- coding:utf-8 -*-

;;; Copyright (c) Eddie.
;;; Distributed under the terms of the MIT License.

(in-package :cl-user)
(defpackage darkmatter.web.handle
  (:use :cl)
  (:import-from :darkmatter.web.render
                :notfound
                :render-index
                :render-files
                :render-notebook)
  (:import-from :quri
                :url-decode)
  (:import-from :alexandria
                :starts-with-subseq)
  (:export :->get
           :->put))
(in-package :darkmatter.web.handle)

(defmacro starts-case (string &rest cases)
  "Call the function with subsequence if the given string starts with pattern within cases."
  (let ((cases (mapcar (lambda (case) (cons (first case) (eval (second case))))
                       cases)))
    `(dolist (case '(,@cases))
       (destructuring-bind (pattern . matched) case
         (if (eq pattern 'otherwise)
             (return (funcall matched ,string))
             (multiple-value-bind (match-p remain)
               (starts-with-subseq pattern ,string :return-suffix t)
               (when match-p (return (funcall matched remain)))))))))

(defun ->get (env)
  "Handle GET requests"
  (let ((uri (url-decode (getf env :request-uri))))
    (starts-case (getf env :request-uri)
      ("/browse/" (lambda (path) (=>browse env path)))
      ("/plugin/" (lambda (path) (=>plugin env path)))
      ("/" (lambda (path) (=>root env path)))
      (otherwise (notfound env)))))

(defun =>browse (env path)
  )

(defun =>plugin (env path)
  )

(defun ->put (env)
  "Handle PUT requests"
  )
