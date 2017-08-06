(ql:quickload :darkmatter-web-server)

(in-package :cl-user)
(defpackage darkmatter.web.app
  (:use :cl)
  (:import-from :darkmatter.web
                :*web*))
(in-package :darkmatter.web.app)

*web*
