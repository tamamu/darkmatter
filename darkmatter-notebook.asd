#|
  This file is a part of darkmatter project.
  Copyright (c) 2017 Eddie (tamamu.1r1s@gmail.com)
|#

#|
  Author: Eddie (tamamu.1r1s@gmail.com)
|#

(in-package :cl-user)
(defpackage darkmatter-notebook-asd
  (:use :cl :asdf))
(in-package :darkmatter-notebook-asd)

(defsystem darkmatter-notebook
  :version "1.0.0"
  :author "Eddie"
  :license "MIT"
  :depends-on (:clack
               :jsonrpc
               :quri
               :djula
               :alexandria
               :yason)
  :components ((:module "src"
                :components
                ((:file "client/client" :depends-on ("client/handler"))
                 (:file "client/handler" :depends-on ("client/render" "client/runtime" "client/user" "utils"))
                 (:file "client/render" :depends-on ("client/user"))
                 (:file "client/user" :depends-on ("settings" "utils"))
                 (:file "client/runtime")
                 (:file "settings")
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
