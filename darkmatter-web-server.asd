#|
  This file is a part of darkmatter project.
  Copyright (c) 2017 Eddie (tamamu.1r1s@gmail.com)
|#

#|
  Author: Eddie (tamamu.1r1s@gmail.com)
|#

(in-package :cl-user)
(defpackage darkmatter-web-server-asd
  (:use :cl :asdf))
(in-package :darkmatter-web-server-asd)

(defsystem darkmatter-web-server
  :version "1.0.0"
  :author "Eddie"
  :license "MIT"
  :depends-on (:clack
               :quri
               :djula
               :alexandria
               :yason)
  :components ((:module "src"
                :components
                ((:file "web" :depends-on ("handle"))
                 (:file "handle" :depends-on ("render" "utils" "web-user"))
                 (:file "settings")
                 (:file "render" :depends-on ("web-user"))
                 (:file "utils")
                 (:file "web-user" :depends-on ("settings")))))
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
