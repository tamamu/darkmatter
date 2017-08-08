#|
  This file is a part of darkmatter project.
  Copyright (c) 2017 Eddie (tamamu.1r1s@gmail.com)
|#

#|
  Author: Eddie (tamamu.1r1s@gmail.com)
|#

(in-package :cl-user)
(defpackage darkmatter-eval-server-asd
  (:use :cl :asdf))
(in-package :darkmatter-eval-server-asd)

(defsystem darkmatter-eval-server
  :version "1.0.0"
  :author "Eddie"
  :license "MIT"
  :depends-on (:jsonrpc)
  :components ((:module "src"
                :components
                ((:file "eval" :depends-on ("rpc"))
                 (:file "rpc" :depends-on ("eval-user" "settings"))
                 (:file "settings" :depends-on ("eval-user" "web-user"))
                 (:file "eval-user")
                 (:file "web-user"))))
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
