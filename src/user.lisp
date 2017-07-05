(in-package :cl-user)
(defpackage darkmatter-user
  (:use :cl)
  (:export :*use-plugin-list*))
(in-package :darkmatter-user)
(defparameter *use-plugin-list*
  (list "plot"))

