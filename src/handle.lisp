;;; handle.lisp
;;-*- coding:utf-8 -*-

;;; Copyright (c) Eddie.
;;; Distributed under the terms of the MIT License.

(in-package :cl-user)
(defpackage darkmatter.web.handle
  (:use :cl)
  (:import-from :darkmatter-user
                :*plugin-handler*)
  (:import-from :darkmatter.web.render
                :notfound
                :render-index
                :render-files
                :render-notebook)
  (:import-from :quri
                :url-decode)
  (:import-from :alexandria
                :starts-with-subseq
                :switch)
  (:export :->get
           :->put))
(in-package :darkmatter.web.handle)

(defparameter +launch-eval-server+
  "ros run --load darkmatter-eval-server -e \"(require :darkmatter-eval-server)\"")

(defun emit-json (json-string)
  `(201 (:content-type "application/json") (,json-string)))

(defun starts-case (keyform cases)
  "Call the function with subsequence if the given string starts with pattern within cases."
  (dolist (case cases)
    (destructuring-bind (pattern matched) case
      (if (eq pattern 'otherwise)
          (return (funcall matched keyform))
          (multiple-value-bind (match-p remain)
            (starts-with-subseq pattern keyform :return-suffix t)
            (when match-p (return (funcall matched remain))))))))

(defun %read-raw-body (env)
  (let ((raw-body (flexi-streams:make-flexi-stream
                    (getf env :raw-body)
                    :external-format (flexi-streams:make-external-format :utf-8)))
        (result (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
    (with-output-to-string (out result)
      (loop for line = (read-line raw-body nil nil) while line
            do (write-line line :output-stream out)))
    result))

(defun ->get (env)
  "Handle GET requests
   * /browse/
   * /plugin/
   * /"
  (let ((uri (url-decode (getf env :request-uri))))
    (starts-case uri
      `(("/browse/" ,(lambda (path) (get/browse/ env path)))
        ("/plugin/" ,(lambda (path) (get/plugin/ env path)))
        ("/" ,(lambda (path) (get/ env path)))
        (otherwise ,(lambda (path) (notfound env)))))))

(defun get/browse/ (env path)
  "Send file to client
   * /browse/.../        File manager
   * /browse/.../*.lisp  Notebook
   * /browse/.../*       File"
  (if (string= path "")
      (notfound env)
      (render-notebook env path)))

(defun get/plugin/ (env path)
  "Send file in the plugin to client
   * /plugin/     Using plugins list
   * /plugin/*/   Plugin detail
   * /plugin/*/*  Plugin file"
  (with-hash-table-iterator
    (generator-fn *plugin-handler*)
    (loop
      (multiple-value-bind (more? key handler)
        (generator-fn)
        (unless more? (return (notfound env)))
        (multiple-value-bind (match? plugin-path)
          (starts-with-subseq key path :return-suffix t)
          (when match?
            (return (funcall handler env plugin-path))))))))

(defun get/ (env path)
  "Send index to client"
  (render-index env))

(defun ->put (env)
  "Handle PUT requests
   * /plugin/
   * /"
  (let* ((uri (url-decode (getf env :request-uri)))
         (body (%read-raw-body env))
         (json (yason:parse body)))
    (if (null (hash-table-p json))
        (emit-json "{}")
        (let ((method (gethash "method" json))
              (params (gethash "params" json (make-hash-table))))
          (starts-case uri
            `(("/plugin/" ,(lambda (path) (put/plugin/ env path method params)))
              ("/" ,(lambda (path) (put/ env path method params)))
              (otherwise ,(lambda (path) (emit-json "{}")))))))))

(defun put/plugin/ (env path method params)
  (with-hash-table-iterator
    (generator-fn *plugin-methods*
      (loop
        (multiple-value-bind (more? key method)
          (generator-fn)
          (unless more? (return (emit-json "{}")))
          (multiple-value-bind (match? plugin-path)
            (starts-with-subseq key path :return-suffix t)
            (when match?
              (return (funcall method env plugin-path params)))))))))

(defun put/ (env path method params)
  "Send procedure result as JSON
   * darkmatter/makeServer

     Response
     {
       'status': boolean,
       'port': integer
     }

   * darkmatter/save

     Response
     {
       'status': boolean,
       'timestamp': string
     }"
  (switch (method :test #'string=)
    ("darkmatter/makeServer" (make-eval-server params))
    ("darkmatter/save" (save params))
    (default (emit-json "{}"))))

(defun make-eval-server (params)
  (let* ((out (make-string-output-stream))
         (proc (uiop:launch-program +launch-eval-server+)))
    (sleep 5)
    (let ((stdout (get-output-stream-string out)))
      (print stdout))))

(defun save (params)
  )
