;;; render.lisp
;;-*- coding:utf-8 -*-

;;; Copyright (c) Eddie.
;;; Distributed under the terms of the MIT License.

(in-package :cl-user)
(defpackage darkmatter.client.render
  (:use :cl)
  (:import-from :darkmatter-client-user
                :*plugin-scripts*)
  (:export :notfound
           :render-index
           :render-files
           :render-notebook

           :darkmatter-web-runtime))
(in-package :darkmatter.client.render)

(defvar *dm-web-store*
  (make-instance 'djula:file-store
                 :search-path
                 (list (asdf:system-relative-pathname "darkmatter-notebook" "templates/"))))

(setf djula:*current-store* *dm-web-store*)

(defvar +index.html+ (djula:compile-template* "index.html"))
(defvar +files.html+ (djula:compile-template* "files.html"))
(defvar +notebook.html+ (djula:compile-template* "notebook.html"))
(defvar +404.html+ (djula:compile-template* "404.html"))

(defun pathext= (path ext)
  (string= (symbol-name ext)
           (string-upcase (pathname-type path))))

(defun darkmatter-web-runtime (env)
  (list :define (format nil "~{<script src=\"~A\"></script>~}"
                        '("/static/HttpRequest.js"
                          "/static/EvalClient.js"))
        :include (format nil "<script>const SERVER_URI = \"http://~A:~A\"; const TOKEN = \"~A\";</script>"
                         ;server uri
                         (getf env :server-name)
                         (getf env :server-port)
                         ;token
                         (write-to-string (get-universal-time)))))

(defun darkmatter-format-p (list)
  (and (listp list) (eq :darkmatter (first list))))

(defun notfound (env)
  (let ((*cunnert-store* *dm-web-store*))
  `(404 (:content-type "text/html")
    (,(djula:render-template* +404.html+ nil)))))

(defun render-index (env)
  (let ((*current-store* *dm-web-store*))
  `(200 (:content-type "text/html")
    (,(djula:render-template* +index.html+ nil)))))

(defun render-files (env path)
  (let ((*current-store* *dm-web-store*))
  `(200 (:content-type "text/html")
    (,(djula:render-template* +files.html+ nil)))))

(defun render-notebook (env path)
  (let ((*current-store* *dm-web-store*)
        (fp (probe-file path))
        (cells nil))
    (when fp
      (with-open-file (in path :direction :input)
        (setf cells (read in))
        (if (darkmatter-format-p cells)
            (setf cells (rest cells))
            (setf cells nil))))
    `(200 (:content-type "text/html")
      (,(djula:render-template* +notebook.html+ nil
                                :cells cells
                                :root (directory-namestring path)
                                :host (getf env :server-name)
                                :port (getf env :server-port)
                                :path path
                                :plugins *plugin-scripts*
                                :token (write-to-string (get-universal-time)))))))
