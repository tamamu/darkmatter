;;; rpc.lisp
;;-*- coding:utf-8 -*-

;;; Copyright (c) Eddie.
;;; Distributed under the terms of the MIT License.

(in-package :cl-user)
(defpackage darkmatter.eval.rpc
  (:use :cl)
  (:import-from :darkmatter.settings
                :*plugins*
                :load-eval-plugins)
  ;(:import-from :darkmatter-hooks
  (:import-from :darkmatter.eval.user
                :*eval-string-before-hooks*
                :*eval-string-after-hooks*
                :*eval-string-finalize-hooks*)
  (:export :+rpcdef-list+
           :*eval-string-before-hooks*
           :*eval-string-after-hooks*
           :*eval-string-finalize-hooks*))
(in-package :darkmatter.eval.rpc)

(defvar +rpcdef-list+
  (list))

(defmacro defrpc (name arglist &body body)
  "Define function as RPC."
  (let ((args (gensym "ARGS")))
    `(progn
       (defun ,name (,args)
         (let ,(mapcar
                (lambda (x)
                  (list x `(gethash ,(symbol-name x) ,args)))
                arglist)
           ,@body))
       (push (cons ,(symbol-name name) (function ,name))
             +rpcdef-list+))))

(defvar *trace-style* :default)

(defvar *default-package-definition*
  (list :use '(:cl)))

(defvar *using-package* nil)

(defun initialize-package ()
  "Make new package."
  (let* ((magic (write-to-string (get-universal-time)))
         (pkg (make-package (format nil "DARKMATTER.LOCAL.~A" magic)
                            :use (getf *default-package-definition* :use))))
    pkg))

(defrpc |darkmatter/initializePackage| ()
  "Call initialize-package.

   * No response"
  (%log "Init package")
  (initialize-package))

(defrpc |darkmatter/initialize| (|processId| |rootUri| |initializeOptions| |trace|)
  "Initialize darkmatter evaluation server.
   processId is the identifier for the instance of this program.

   * No response"
  ;(let ((plugins (gethash |plugins| |initializeOptions|)))
  ;  (when plugins
  ;    (mapcar #'load plugins)))
  (load-eval-plugins)

  (let ((default-package (gethash :|defaultPackage| |initializeOptions|)))
    (when default-package
      (setf (getf *default-package-definition* :use)
            default-package)))

  (when |trace|
    (setf *trace-style* |trace|)))

(defun %log (output &key (stream *standard-output*))
  (multiple-value-bind (sec min hour date mon year day dst-p tz)
    (get-decoded-time)
    (declare (ignore day dst-p tz))
    (format stream "[~d/~d/~2,'0d] ~2,'0d:~2,'0d:~2,'0d~%~A~%"
            year mon date hour min sec output)))

(defun plist->hash (plist)
  "convert plist to hash-table"
  (let ((hash (make-hash-table :test #'equal)))
    (loop for (key value) on plist by #'cddr while value
          do (setf (gethash (symbol-name key) hash) value))
    hash))

(defun %hook-eval-string-before (sexp)
  (reduce #'funcall
          *eval-string-before-hooks*
          :initial-value sexp
          :from-end t))

(defun %hook-eval-string-after (return-value)
  (reduce #'funcall
          *eval-string-after-hooks*
          :initial-value return-value
          :from-end t))

(defun %hook-eval-string-finalize (return-value output-rendering cellId)
  (dolist (hook *eval-string-finalize-hooks*)
    (funcall hook return-value output-rendering cellId)))

(defrpc |darkmatter/eval| (|code| |outputRendering| |cellId|)
  "Evaluate the string from the editor.

   * Response result"
  (%log (format nil "Eval: ~A" |code|))

  (let* ((*package* (or *using-package*
                        (initialize-package)))
         (server-output *standard-output*)
         (*standard-output* (make-string-output-stream))
         (*error-output* (make-string-output-stream))
         (*trace-output* (make-string-output-stream))
         (code-position 0)
         (return-value nil)
         (errorp nil))

    (handler-case
      (loop while code-position
            with sexp
            do (multiple-value-setq (sexp code-position)
                 (read-from-string |code| :eof-error-p t :start code-position))
               (setf sexp (%hook-eval-string-before sexp)
                     return-value (%hook-eval-string-after (eval sexp))))
      (end-of-file (c) nil)
      (error (c) (setf errorp t)
                 (write c)))

    (setf *using-package* *package*)
    (let* ((standard-output (get-output-stream-string *standard-output*))
           (error-output (get-output-stream-string *error-output*))
           (trace-output (get-output-stream-string *trace-output*))
           (all-output (format nil "~A~A~A"
                       trace-output
                       error-output
                       standard-output))
           (*package* (find-package :darkmatter.eval.rpc)))

      (%log (format nil "Result: ~A~%~A" return-value all-output) :stream server-output)

      (%hook-eval-string-finalize return-value |outputRendering| |cellId|)
      (plist->hash
        `(:|returnValue| ,(write-to-string return-value)
          :|output| ,all-output)))))


(defrpc |darkmatter/getShareObject| ()
  "Get a share object of the asynchronous task of the id.

   * Response object"
  )
