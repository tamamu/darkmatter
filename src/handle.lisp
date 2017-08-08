;;; handle.lisp
;;-*- coding:utf-8 -*-

;;; Copyright (c) Eddie.
;;; Distributed under the terms of the MIT License.

(in-package :cl-user)
(defpackage darkmatter.web.handle
  (:use :cl)
  (:import-from :darkmatter.web.user
                :*plugin-handler*
                :*plugin-methods*)
  (:import-from :darkmatter.settings
                :*plugins*)
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
  (:export :kill-all-process
           :->get
           :->put))
(in-package :darkmatter.web.handle)

(defparameter +launch-eval-server+
  ;"ros run --load darkmatter-eval-server.asd -e \"(require :darkmatter-eval-server)\""
  "./roswell/darkmatter-eval-server.ros")

(defvar *eval-server-processes*
  (make-hash-table :test #'equalp))

(defun entry-client (id)
  (let ((proc-list (gethash id *eval-server-processes* nil)))
  (if proc-list
      proc-list
      (setf (gethash id *eval-server-processes*)
            (make-hash-table :test #'equalp)))))

(defun kill-all-process ()
  (with-hash-table-iterator
    (generator-fn *eval-server-processes*)
      (loop
        (multiple-value-bind (more? key clnt-tbl)
          (generator-fn)
          (unless more? (return t))
          (with-hash-table-iterator
            (generator-fn clnt-tbl)
            (loop
              (multiple-value-bind (more? key proc)
                (generator-fn)
                (unless more? (return t))
                (uiop:terminate-process proc :urgent t)
                (uiop:terminate-process proc :urgent t)
                (uiop:wait-process proc)))))))
  (format t "Killed all eval server processes."))

(defun emit-response (id plist)
  (%emit-json
    201
    (format nil "{\"jsonrpc\": \"2.0\", \"result\": {~{\"~A\": ~S~^, ~}}, \"id\": ~S}"
            plist
            id)))

(defun emit-error (id code)
  (let ((code (case code
                 (:parse-error -32700)
                 (:invalid-request -32600)
                 (:method-not-found -32601)
                 (:invalid-params -32602)
                 (:internal-error -32603)
                 (otherwise code)))
        (message (case code
                   (:parse-error "Parse error")
                   (:invalid-request "Invalid Request")
                   (:method-not-found "Method not found")
                   (:invalid-params "Invalid params")
                   (:internal-error "Internal error")
                   (otherwise "Server error"))))
  (%emit-json
    200
    (format nil "{\"jsonrpc\": \"2.0\", \"error\": {\"code\": ~S, \"message\": ~S}, \"id\": ~S}"
            code
            message
            id))))

(defun starts-case (keyform cases)
  "Call the function with subsequence if the given string starts with pattern within cases."
  (dolist (case cases)
    (destructuring-bind (pattern matched) case
      (if (eq pattern 'otherwise)
          (return (funcall matched keyform))
          (multiple-value-bind (match-p remain)
            (starts-with-subseq pattern keyform :return-suffix t)
            (when match-p (return (funcall matched remain))))))))

(defun %emit-json (status json-string)
  `(,status (:content-type "application/json") (,json-string)))

(defun %parse-port-number (string)
  (dotimes (index (length string))
    (when (eq #\: (char string index))
      (return (parse-integer string :start (1+ index) :junk-allowed t)))))

(defun %read-raw-body (env)
  (let ((raw-body (flexi-streams:make-flexi-stream
                    (getf env :raw-body)
                    :external-format (flexi-streams:make-external-format :utf-8)))
        (result (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
    (with-output-to-string (out result)
      (loop for line = (read-line raw-body nil nil) while line
            do (write-line line out)))
    result))

(defun ->get (env)
  "Handle GET requests
   * /browse/
   * /plugin/
   * /"
  (let ((uri (url-decode (getf env :request-uri))))
    (format t "[GET] ~A~%" uri)
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
  (if (= (length path) 0)
      `(200 (:content-type "text/html")
        (,(format nil "~A" *plugins*)))
      (with-hash-table-iterator
        (generator-fn *plugin-handler*)
        (loop
          (multiple-value-bind (more? plugin-name handler)
            (generator-fn)
            (unless more? (return (notfound env)))
            (multiple-value-bind (match? rest-path)
              (starts-with-subseq (format nil "~A/" plugin-name) path :return-suffix t)
              (format t "[~A] ~A(~A)~%" plugin-name path rest-path)
              (when match?
                (if (= (length rest-path) 0)
                    (return `(200 (:content-type "text/html") (,(format nil "~A" plugin-name))))
                    (let ((response (funcall handler env rest-path)))
                      (return (or response (notfound env))))))))))))

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
    (format t "[PUT] ~A~%" uri)
    (force-output)
    (if (null (hash-table-p json))
        (emit-error (gethash "id" json "null") :parse-error)
        (let ((id (gethash "id" json "null"))
              (method (gethash "method" json))
              (params (gethash "params" json (make-hash-table))))
          (entry-client id)
          (format t "[PUT] from ~A to ~A~%" id uri)
          (starts-case uri
            `(("/plugin/" ,(lambda (path) (put/plugin/ env path id method params)))
              ("/" ,(lambda (path) (put/ env path id method params)))
              (otherwise ,(lambda (path) (emit-error id :method-not-found)))))))))

(defun put/plugin/ (env path id method params)
  (with-hash-table-iterator
    (generator-fn *plugin-methods*)
      (loop
        (multiple-value-bind (more? key plugin-entry)
          (generator-fn)
          (unless more? (return (emit-error id :invalid-request)))
          (multiple-value-bind (match? plugin-path)
            (starts-with-subseq key path :return-suffix t)
            (when match?
              (let ((method (gethash method plugin-entry)))
                (if method
                    (return (emit-response id (funcall method params)))
                    (return (emit-error id :method-not-found))))))))))

(defun put/ (env path id method params)
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
    ("darkmatter/makeServer" (make-eval-server id params))
    ("darkmatter/save" (save id params))
    (:default (emit-error id :method-not-found))))

(defun make-eval-server (id params)
  (let* ((proc-list (gethash id *eval-server-processes*))
         (descripter (gethash "descripter" params "none"))
         (out (make-array 0 :element-type 'character :adjustable t))
         (proc (uiop:launch-program +launch-eval-server+ :output :stream))
         (port nil))
    (setf (gethash descripter proc-list) proc)
    (format t "[darkmatter/makeServer] ")
    (loop for cnt from 0 below 50
          until (numberp port)
          do (format t ".")
             (force-output)
             (sleep 2)
             (setf out (read-line (uiop:process-info-output proc)))
             (setf port (%parse-port-number out)))
      (if (numberp port)
          (progn
            (format t "Make successful in port ~A~%" port)
            (emit-response id `("status" "true" "port" ,port)))
          (progn
            (format t "Failed (~A)~%" port)
            (emit-error id 32000)))))

(defun save (params)
  )
