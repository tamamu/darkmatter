;;; darkmatter.lisp
;;-*- coding:utf-8 -*-

;;; Copyright (c) Eddie.
;;; Distributed under the terms of the MIT License.

(in-package :cl-user)
(defpackage darkmatter
  (:use :cl :darkmatter.rpc)
  (:import-from :cl-fad
                :with-open-temporary-file)
  (:export :start))
(in-package :darkmatter)

(defparameter *log-directory* (car (directory "/tmp/darkmatter*")))
(when (null *log-directory*)
  (setf *log-directory*
        (format nil "/tmp/darkmatter~A/"
                (with-output-to-string (stream)
                  (let ((*print-base* 36))
                    (dotimes (i 6) (princ (random 36) stream))))))
  (ensure-directories-exist *log-directory*))

(defun count-log-files ()
  (length
    (directory
      (format nil "~A*" *log-directory*))))

(defun next-log-file-number ()
  (let* ((largest
           (car
             (last
               (directory
                 (format nil "~A*" *log-directory*))))))
    (if (null largest)
        0
        (1+ (parse-integer (file-namestring largest))))))

(defvar *server* (jsonrpc:make-server))

(mapcar
  #'(lambda (rpcdef)
      (jsonrpc:expose *server*
                      (car rpcdef)
                      (cdr rpcdef)))
  +rpcdef-list+)

(defun start ()
  (let* ((port 50000)
         (log-template (format nil "~A%" *log-directory*)))
    (with-open-temporary-file
      (logfile :direction
               :output
               :template
               log-template
               :generate-random-string
               #'(lambda () (write-to-string (next-log-file-number))))
      (format t "Start darkmatter with logging into ~A~%"
              (truename logfile))
      (let ((*standard-output* logfile))
        (tagbody
          start
          (handler-case
            (progn
              (format t "#~A~%" port)
              (force-output)
              (jsonrpc:server-listen *server*
                                     :mode :websocket
                                     :port port
                                     :silent t))
            ((or usocket:address-in-use-error error) (c)
              (incf port)
              (if (< 65535 port)
                  (progn
                    (format t "#EXIT~%")
                    (force-output)
                    (go finish))
                  (progn
                    (format t "#FAILURE~%")
                    (force-output)
                    (go start)))))
          finish)
        (when (zerop (count-log-files))
          (uiop:delete-empty-directory (cl-fad:pathname-as-file *log-directory*)))))))
