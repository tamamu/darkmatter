;;; rpc.lisp
;;-*- coding:utf-8 -*-

;;; Copyright (c) Eddie.
;;; Distributed under the terms of the MIT License.

(in-package :cl-user)
(defpackage darkmatter.rpc
  (:use :cl)
  (:import-from :darkmatter.utils
                :gen-serial
                :escape)
  (:import-from :darkmatter.settings
                :*plugins*
                :load-settings)
  (:import-from :darkmatter-user
                :*debug*
                :*eval-string-before-hooks*
                :*eval-string-after-hooks*
                :*eval-string-finalize-hooks*
                :load-eval-plugins)
  (:import-from :alexandria
                :hash-table-plist
                :plist-hash-table)
  (:export :+rpcdef-list+
           :*using-package*))
(in-package :darkmatter.rpc)

(defvar pkgnum (gen-serial 0))
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
  (let* ((pkg (make-package (format nil "DARKMATTER.LOCAL.~A" (funcall pkgnum))
                            :use (getf *default-package-definition* :use))))
    pkg))

(defrpc |darkmatter/initializePackage| ()
  "Call initialize-package.

   * No response"
  (%log "Init package")
  (initialize-package))

(defrpc |darkmatter/initialize| (|initializeOptions| |trace|)
  "Initialize darkmatter evaluation server.
   processId is the identifier for the instance of this program.

   initializeOptions: {
     debug: Boolean,
     ignoreSettings: Boolean,
     plugins: String,
     defaultPackage: String
   }

   * Response whether suuceeded initialization or not"
  (%log (format nil "Initalize (debug:~A)" *debug*))

  (let ((debug (gethash "debug" |initializeOptions| nil)))
    (if debug
        (format t "[Option] Enable debug mode~%")
        (format t "[Option] Disable debug mode~%"))
    (setf *debug* debug))

  (let ((ignore-settings (gethash "ignoreSettings" |initializeOptions| nil)))
    (when (not ignore-settings)
      (format t "[Option] Load settings~%")
      (load-settings)))

  (let ((plugins (gethash "plugins" |initializeOptions|)))
    (when plugins
      (format t "[Option] Get plugins ~A~%" plugins)
      (load-eval-plugins plugins)))

  (let ((default-package (gethash "defaultPackage" |initializeOptions|)))
    (when default-package
      (let ((default-package (eval (read-from-string default-package))))
        (format t "[Option] Get defaultPackage ~A~%" default-package)
        (setf (getf *default-package-definition* :use)
              default-package))))

  (setf *using-package* (initialize-package))

  (force-output)

  (when |trace|
    (setf *trace-style* |trace|))

  (plist-hash-table
    (list "initialized" t)
    :test #'equal))

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

(defun %hook-eval-string-before (sexp optional)
  (let ((sexp sexp)
        (optional optional))
    (dolist (hook *eval-string-before-hooks*)
      (multiple-value-setq (sexp optional)
        (funcall hook sexp optional)))
    (values sexp optional)))

(defun %hook-eval-string-after (return-value optional)
  (let ((return-value return-value)
        (optional optional))
    (dolist (hook *eval-string-after-hooks*)
      (multiple-value-setq (return-value optional)
        (funcall hook return-value optional)))
    (values return-value optional)))

(defun %hook-eval-string-finalize (return-value output-rendering optional)
  (let ((return-value return-value)
        (output-rendering output-rendering)
        (optional optional))
    (dolist (hook *eval-string-finalize-hooks*)
      (multiple-value-setq (return-value output-rendering optional)
        (funcall hook return-value output-rendering optional)))
    (values return-value output-rendering optional)))

(defrpc |darkmatter/eval| (|code| |outputRendering| |cellId| |optional|)
  "Evaluate the string from the editor.

   * Response result"
  (%log (format nil "Eval: ~A" |code|))

  (if (null |optional|)
      (setf |optional| (make-hash-table :test #'equalp)))

  (let* ((*package* (or *using-package*
                        (initialize-package)))
         (server-output *standard-output*)
         (*standard-output* (make-string-output-stream))
         (*error-output* (make-string-output-stream))
         (*trace-output* (make-string-output-stream))
         (code-position 0)
         (source '(progn))
         (return-value nil)
         (errorp nil))

    ;; parse and concatenate the source in a PROGN
    (handler-case
      (loop while code-position
            with sexp
            do (multiple-value-setq (sexp code-position)
                 (read-from-string |code| :eof-error-p t :start code-position))
            always sexp
            do (setf source (append source (list sexp))))
      (end-of-file (c) nil))

    ;; eval the source with hooks
    (handler-case
      (progn
        (multiple-value-setq (source |optional|)
          (%hook-eval-string-before source |optional|))
        (multiple-value-setq (return-value |optional|)
          (%hook-eval-string-after (eval source) |optional|)))
      (error (c)
        (setf errorp t)
        (format t "<strong class=\"error\">~A</strong>~%" (escape (write-to-string c)))))

    (multiple-value-setq (return-value |outputRendering| |optional|)
      (%hook-eval-string-finalize return-value |outputRendering| |optional|))

    (setf *using-package* *package*)
    (let* ((standard-output (get-output-stream-string *standard-output*))
           (error-output (get-output-stream-string *error-output*))
           (trace-output (get-output-stream-string *trace-output*))
           (all-output (format nil "~A~A~A"
                       trace-output
                       error-output
                       standard-output))
           (*package* (find-package :darkmatter.rpc)))

      (%log (format nil "Result: (Return) ~A~%(Output) ~A~%(Optional) ~A~%"
                    return-value
                    all-output
                    |optional|)
            :stream server-output)

      (plist-hash-table
        `("returnValue" ,(write-to-string return-value)
          "output" ,all-output
          "optional" ,|optional|)))))


(defrpc |darkmatter/getShareObject| ()
  "Get a share object of the asynchronous task of the id.

   * Response object"
  )
