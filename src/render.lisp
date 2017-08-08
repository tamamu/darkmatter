;;; render.lisp
;;-*- coding:utf-8 -*-

;;; Copyright (c) Eddie.
;;; Distributed under the terms of the MIT License.

(in-package :cl-user)
(defpackage darkmatter.web.render
  (:use :cl)
  (:import-from :darkmatter.web.user
                :*plugin-scripts*)
  (:export :notfound
           :render-index
           :render-files
           :render-notebook))
(in-package :darkmatter.web.render)


(djula:add-template-directory
  (asdf:system-relative-pathname "darkmatter-web-server" "templates/"))

(defvar +index.html+ (djula:compile-template* "index.html"))
(defvar +files.html+ (djula:compile-template* "files.html"))
(defvar +notebook.html+ (djula:compile-template* "notebook.html"))
(defvar +404.html+ (djula:compile-template* "404.html"))

(defun pathext= (path ext)
  (string= (symbol-name ext)
           (string-upcase (pathname-type path))))

(defun darkmatter-format-p (list)
  (and (listp list) (eq :darkmatter (first list))))

(defun notfound (env)
  `(404 (:content-type "text/html")
    (,(djula:render-template* +404.html+ nil))))

(defun render-index (env)
  `(200 (:content-type "text/html")
    (,(djula:render-template* +index.html+ nil))))

(defun render-files (env path)
  `(200 (:content-type "text/html")
    (,(djula:render-template* +files.html+ nil))))

(defun render-notebook (env path)
  (let ((fp (probe-file path)))
    (let ((cells nil))
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
                                  :token (write-to-string (get-universal-time))))))))
