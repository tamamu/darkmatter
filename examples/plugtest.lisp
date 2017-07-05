;;
;; Example Plugin
;;
;; Write this file path in your .darkmatter.conf and try it!
;; The most generic way is to eval below function.
;;   (asdf:system-relative-pathname :darkmatter "examples/plugtest.lisp")
;;

;; You must start (in-packgae :darkmatter-user)
(in-package :darkmatter-user)

;; defpackage is better, but it's not required.
(defpackage plugtest
  (:use :cl :darkmatter-user :cl-markup))
(in-package :plugtest)

(defun handle (env path)
  ;; Show the path client requested.
  (format t "PATH:~A~%" path)
  ;; Receive a file depends on the path.
  (if (string= path "/test")
    ;; Exactly /plugin/plug/test
    `(200 (:content-type "text/html")
      (,(markup
          (a :href "https://github.com/tamamu/darkmatter" "Get Darkmatter!!"))))
    ;; /plugin/plug + something
    `(200 (:content-type "text/javascript")
      ("alert('This is a test plugin.');"))))

;; !!DON'T FORGET TO REGIST YOUR PLUGIN!!
(regist-plugin-handler "plug" #'handle)
(regist-plugin-script "plug" "hoge.js")
