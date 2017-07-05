(in-package :cl-user)
(defpackage darkmatter.settings
  (:use :cl)
  (:export :get-plugin-list))
(in-package :darkmatter.settings)

(defun get-plugin-list ()
  '("test1"))
