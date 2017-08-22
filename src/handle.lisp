;;; handle.lisp
;;-*- coding:utf-8 -*-

;;; Copyright (c) Eddie.
;;; Distributed under the terms of the MIT License.

(in-package :cl-user)
(defpackage darkmatter.web.handle
  (:use :cl)
  (:import-from :darkmatter.web.user
                :*plugin-handler*
                :*plugin-methods*
                :load-web-plugins)
  (:import-from :darkmatter.settings
                :*plugins*)
  (:import-from :darkmatter.utils
                :starts-case
                :%log)
  (:import-from :darkmatter.web.render
                :notfound
                :render-index
                :render-files
                :render-notebook)
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
(in-package :darkmatter.web.handle)

(load-web-plugins)

(defparameter +launch-eval-server+
  (if (= 0 (third (multiple-value-list (uiop:run-program "which dmserver" :ignore-error-status t))))
      "dmserver"
      (format nil "exec ~A"
              (asdf:system-relative-pathname "darkmatter-web-server" #P"roswell/dmserver.ros"))))
(%log (format nil "Found ~A as Eval server" +launch-eval-server+))

(defstruct eval-server-info
  (process nil)
  (port 0 :type integer))

(defvar *client* (jsonrpc:make-client))

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
              (multiple-value-bind (more? key info)
                (generator-fn)
                (unless more? (return t))
                (uiop:terminate-process (eval-server-info-process info) :urgent t)
                (uiop:wait-process (eval-server-info-process info))))))))
  (%log "Killed all eval server processes."))

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
    (%log (format nil "~A ~A%" id code) "Error response")
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
          (entry-client id)
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
  (let ((proc-list (gethash id *eval-server-processes*)))
    (if (null descripter)
        (emit-error id :invalid-params)
        (progn
          (tagbody start
                   (handler-bind ((error (lambda (c)
                                           (format t "~A~%" c)
                                           (make-eval-server id descripter)
                                           (go start))))
                     (jsonrpc:client-connect *client*
                                             :url (format nil "ws://127.0.0.1:~A"
                                                          (eval-server-info-port (gethash descripter proc-list)))
                                             :mode :websocket)))
          (%log (format nil "Method ~A~%Params ~A"
                        method
                        (hash-table-plist params)))
          (force-output)
          (let* ((result (jsonrpc:call *client* method params))
                 (json-string (encode-to-string result)))
            (%log (format nil "Result ~A" (response-result id json-string)))
            `(200 (:content-type "application/json")
              (,(response-result id json-string))))))))

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

(defun make-eval-server (id descripter)
  (let* ((proc-list (gethash id *eval-server-processes*))
         (out (make-array 0 :element-type 'character :adjustable t))
         (proc (uiop:launch-program +launch-eval-server+ :output :stream))
         (port nil))
    (%log "Try connecting to eval server")
    (loop for cnt from 0 below 50
          until (numberp port)
          do (format t ".")
             (force-output)
             (sleep 2)
             (setf out (read-line (uiop:process-info-output proc)))
             (setf port (%parse-port-number out)))
    (fresh-line)
    (if (numberp port)
        (%log (format nil "Success to connect in port ~A" port))
        (%log "Failed to connect"))
    (setf (gethash descripter proc-list)
          (make-eval-server-info :process proc :port port))))

#|
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
            (emit-error id -32000)))))
|#

(defun save (params)
  )
