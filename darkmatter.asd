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
  :version "0.0.1"
  :author "Eddie"
  :license "MIT"
  :depends-on (:clack
               :websocket-driver-server
							 :jsown
               :djula
               :string-case
               :alexandria
               :cl-markup)
  :components ((:module "src"
                :components
                ((:file "darkmatter" :depends-on ("infix"))
                 (:file "infix"))))
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
