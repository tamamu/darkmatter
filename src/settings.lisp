(in-package :cl-user)
(defpackage darkmatter.settings
  (:use :cl)
  (:export :get-plugin-list))
(in-package :darkmatter.settings)

(defparameter *plugin-lisp*
  (list (asdf:system-relative-pathname :darkmatter "src/plot.lisp")))

(let ((path (merge-pathnames ".darkmatter.conf" (user-homedir-pathname))))
  (if (probe-file path)
    (load path)))

(mapcar #'load *plugin-lisp*)
