;;; darkmatter.lisp
;;-*- coding:utf-8 -*-

;;; Copyright (c) Eddie.
;;; Distributed under the terms of the MIT License.

(in-package :cl-user)
(defpackage darkmatter
  (:use :cl :darkmatter.rpc)
  (:export :start))
(in-package :darkmatter)

(defvar *server* (jsonrpc:make-server))

(mapcar
  #'(lambda (rpcdef)
      (jsonrpc:expose *server*
                      (car rpcdef)
                      (cdr rpcdef)))
  +rpcdef-list+)

(defun start (&key (port nil))
  (if port
      (jsonrpc:server-listen *server* :mode :websocket :port port)
      (jsonrpc:server-listen *server* :mode :websocket)))
