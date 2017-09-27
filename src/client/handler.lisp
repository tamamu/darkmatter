;;; handler.lisp
;;-*- coding:utf-8 -*-

;;; Copyright (c) Eddie.
;;; Distributed under the terms of the MIT License.

(in-package :cl-user)
(defpackage darkmatter.client.handler
  (:use :cl)
  (:import-from :darkmatter-client-user
                :*plugin-handler*
                :*plugin-methods*
                :load-web-plugins)
  (:import-from :darkmatter.settings
                :*plugins*)
  (:import-from :darkmatter.utils
                :starts-case
                :%log)
  (:import-from :darkmatter.client.render
                :notfound
                :render-index
                :render-files
                :render-notebook)
  (:import-from :darkmatter.client.runtime
                :+launch-server+
                :get-entity
                :get-port
                :make-server-process-table
                :make-server-process
                :delete-server-process-table
                :add-host
                :get-proc)
  (:import-from :quri
                :url-decode)
  (:import-from :alexandria
                :starts-with-subseq
                :plist-hash-table
                :hash-table-plist
                :switch)
  (:export :kill-all-process
           :->get
           :->put))
(in-package :darkmatter.client.handler)

;; Prepare
(load-web-plugins)
(%log (format nil "Found ~A as Eval server" +launch-server+))
;; ---

(defvar *client* (jsonrpc:make-client))

(defvar *server-table*
  (make-server-process-table))

(defun kill-all-process ()
  (delete-server-process-table *server-table*)
  (%log "Killed all server process"))

(defun response-result (id result)
  (format nil "{\"jsonrpc\": \"2.0\", \"id\": ~S, \"result\": ~A}" id result))

(defun emit-response (id hash)
  (%emit-json
    201
    (yason:encode-plist
      `("jsonrpc" "2.0"
        "result" ,hash
        "id" ,id))))

(defun emit-error (id code)
  (let* ((code (case code
                 (:parse-error -32700)
                 (:invalid-request -32600)
                 (:method-not-found -32601)
                 (:invalid-params -32602)
                 (:internal-error -32603)
                 (otherwise code)))
         (message (case code
                    (-32700 "Parse error")
                    (-32600 "Invalid Request")
                    (-32601 "Method not found")
                    (-32602 "Invalid params")
                    (-32603 "Internal error")
                    (otherwise "Server error"))))
    (%log (format nil "~A ~A~%" id code) "Error response")
  (%emit-json
    200
    (encode-to-string
      (plist-hash-table
        `("jsonrpc" "2.0"
          "error" ,(plist-hash-table
                     `("code" ,code
                       "message" ,message)
                     :test #'equal)
          "id" ,id)
        :test #'equal)))))

(defun hash-table-plist-recur (hash-table)
  (let ((plist (hash-table-plist hash-table)))
    (mapcar #'(lambda (elm)
                (if (hash-table-p elm)
                    (hash-table-plist-recur elm)
                    elm))
            plist)))

(defun %emit-json (status json-string)
  `(,status (:content-type "application/json") (,json-string)))

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
    (%log uri :GET)
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
      (let ((real-path (format nil "~A~A" (truename "./") path)))
        (render-notebook env real-path))))

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
              (%log (format nil "~A ~A ~A" plugin-name path rest-path) "GET plugin")
              (when match?
                (let ((response (funcall handler env rest-path)))
                  (return (or response (notfound env)))))))))))

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
    (%log uri :PUT)
    (force-output)
    (if (null (hash-table-p json))
        (emit-error (gethash "id" json "null") :parse-error)
        (let ((id (gethash "id" json "null"))
              (method (gethash "method" json))
              (params (gethash "params" json (make-hash-table)))
              (descripter (gethash "descripter" json)))
          (add-host *server-table* id)
          (starts-case uri
            `(("/plugin/" ,(lambda (path) (put/plugin/ env path id descripter method params)))
              ("/eval/" ,(lambda (path) (put/eval/ env path id descripter method params)))
              ("/" ,(lambda (path) (put/ env path id descripter method params)))
              (otherwise ,(lambda (path) (emit-error id :method-not-found)))))))))

(defun put/plugin/ (env path id descripter method params)
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


(defun put/eval/ (env path id descripter method params)
  (if (null descripter)
      (emit-error id :invalid-params)
      (progn
        (tagbody start
                 (handler-bind ((error (lambda (c)
                                         (format t "~A~%" c)
                                         (make-server-process *server-table* id descripter)
                                         (go start))))
                   (let ((proc (get-proc *server-table* id descripter)))
                     (if proc
                         (progn
                           (%log (format nil "Connect in port ~A" (get-port proc)))
                           (jsonrpc:client-connect *client*
                                                   :url (format nil "ws://127.0.0.1:~A" (get-port proc))
                                                   :mode :websocket))
                         (progn
                           (make-server-process *server-table* id descripter)
                           (go start))))))
        (%log (format nil "Method ~A~%Params ~A"
                      method
                      (hash-table-plist params)))
        (force-output)
        (handler-case
          (let* ((result (jsonrpc:call *client* method params))
                 (json-string (encode-to-string result)))
            (%log (format nil "Result ~A" (response-result id json-string)))
            `(200 (:content-type "application/json")
              (,(response-result id json-string))))
          (error (c)
                 (let ((entity (get-entity (get-proc *server-table* id descripter))))
                   (loop for line = (read-line (uiop:process-info-output entity))
                         while line
                         do (format t "## ~A~%" line))
                   (force-output)
                   (exit)))))))

(defun encode-to-string (ht)
  (let ((stream (make-string-output-stream)))
    (yason:encode ht stream)
    (get-output-stream-string stream)))

(defun encode-plist-to-string (ht)
  (let ((stream (make-string-output-stream)))
    (yason:encode-plist ht stream)
    (get-output-stream-string stream)))


(defun put/ (env path id descripter method params)
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
    ("darkmatter/save" (save id params))
    (:default (emit-error id :method-not-found))))

(defun save (params)
  )
