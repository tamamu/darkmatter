(in-package :cl-user)
(defpackage darkmatter-user
  (:use :cl)
  (:export :*plugin-handler*
           :*plugin-scripts*
           :regist-plugin-handler
           :regist-plugin-script))
(in-package :darkmatter-user)

(defparameter *plugin-handler*
  (make-hash-table :test #'equalp))

(defparameter *plugin-scripts*
  (list))

(defun regist-plugin-handler (name handler)
  (setf (gethash name *plugin-handler*)
        handler))

(defun regist-plugin-script (name uri)
  (push `(("name" . ,name) ("path" . ,uri))
        *plugin-scripts*))


