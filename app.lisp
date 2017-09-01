;(ql:quickload :darkmatter-notebook)

(in-package :cl-user)
(defpackage darkmatter.client.app
  (:use :cl)
  (:import-from :darkmatter.client
                :*web*))
(in-package :darkmatter.client.app)

*web*
