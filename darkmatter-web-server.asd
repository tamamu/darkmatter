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
               :alexandria)
  :components ((:module "src"
                :components
                ((:file "web" :depends-on ("handle"))
                 (:file "handle" :depends-on ("render" "user"))
                 (:file "render" :depends-on ("user"))
                 (:file "user"))))
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
