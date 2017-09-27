#|
  This file is a part of darkmatter project.
  Copyright (c) 2017 Eddie (tamamu.1r1s@gmail.com)
|#

#|
  Author: Eddie (tamamu.1r1s@gmail.com)
|#

(in-package :cl-user)
(defpackage darkmatter-asd
  (:use :cl :asdf))
(in-package :darkmatter-asd)

(defsystem darkmatter
  :version "1.0.0"
  :author "Eddie"
  :license "MIT"
  :depends-on (:cl-fad
               :jsonrpc
               :alexandria)
  :components ((:module "src"
                :components
                ((:file "server/darkmatter" :depends-on ("server/rpc"))
                 (:file "server/rpc" :depends-on ("server/user" "utils"))
                 (:file "server/user" :depends-on ("settings"))
                 (:file "settings" :depends-on ("utils"))
                 (:file "utils"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op darkmatter-test))))
