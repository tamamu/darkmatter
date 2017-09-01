(in-package :cl-user)
(defpackage darkmatter.rpc-test
  (:use :cl
        :prove
        :alexandria))
(in-package :darkmatter.rpc-test)

(defun test (port)
  (let ((client (jsonrpc:make-client)))

  (plan nil)

  (jsonrpc:client-connect client
                          :url (format nil "ws://127.0.0.1:~A" port)
                          :mode :websocket)

  (jsonrpc:call client "darkmatter/eval"
                (plist-hash-table
                  (list "code" "(print *package*)")))

  (finalize)))
