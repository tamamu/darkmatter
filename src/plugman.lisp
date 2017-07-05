(in-package :cl-user)
(defpackage darkmatter.plugman
  (:use :cl)
  (:import-from :alexandria
                :starts-with-subseq)
  (:import-from :darkmatter.settings
                :get-plugin-list)
  (:export :*plugin-handler*
           :*plugin-scripts*
           :regist-plugin-handler
           :get-plugin))
(in-package :darkmatter.plugman)

(defparameter *use-plugin-list*
  (get-plugin-list))

(defparameter *plugin-handler*
  (make-hash-table :test #'equalp))

(defparameter *plugin-scripts*
  (list))

(defun regist-plugin-handler (name handler)
  (setf (gethash name *plugin-handler*)
        handler))

(defun plugin-handle (env path)
  `(200 (:content-type "text/html") ("plugin test")))

(regist-plugin-handler "test" #'plugin-handle)

(defun get-plugin (env path)
  (print *plugin-handler*)
  (format t "GET plugin ~A~%" path)
  (with-hash-table-iterator (generator-fn *plugin-handler*)
    (loop
      (multiple-value-bind (more? key handler) (generator-fn)
        (unless more? (return))
        (multiple-value-bind (match? path)
          (starts-with-subseq key path)
          (when (and match?
                     (member key *use-plugin-list* :test #'equalp))
              (return (funcall handler env path))))))))


