(ql:quickload :darkmatter)

(in-package :cl-user)
(defpackage darkmatter.app
  (:use :cl)
  (:import-from :darkmatter
                :*eval-server*))
(in-package :darkmatter.app)

;(clack:clackup *eval-server* :server :woo :port 8888)
*eval-server*
