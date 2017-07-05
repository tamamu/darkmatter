(in-package :cl-user)
(defpackage darkmatter.serve
  (:use :cl)
  (:import-from :djula
                :add-template-directory
                :compile-template*
                :render-template*)
  (:import-from :string-case
                :string-case)
  (:import-from :alexandria
                :if-let
                :read-file-into-string
                :starts-with-subseq)
  (:import-from :darkmatter.pacman
                :make-temporary-package)
  (:import-from :darkmatter.plugman
                :*plugin-scripts*)
  (:export :read-file
           :serve-index
           :read-global-file
           :get-editable-file
           :notfound))
(in-package :darkmatter.serve)

(defparameter *static-directory*
  (asdf:system-relative-pathname "darkmatter" "static/"))

(djula:add-template-directory (asdf:system-relative-pathname "darkmatter" "templates/"))
(defparameter +base.html+ (djula:compile-template* "base.html"))



(defun read-file (env path)
  (format t "read: ~A~%" path)
  (let ((mime (gethash "content-type" (getf env :headers))))
    (with-open-file (stream path :direction :input :if-does-not-exist nil)
      `(200 (:content-type ,mime
             :content-length ,(file-length stream))
        ,(pathname path)))))

(defun serve-index ()
  `(200 (:content-type "text/html")
    (,(read-file-into-string (merge-pathnames *static-directory* "index.html")))))

(defun read-global-file (env path)
  (let ((fp (probe-file path)))
    (if (string= "LISP" (string-upcase (pathname-type path)))
      (if fp
          (get-editable-file env path)
          (new-editable-file env path))
      (if fp
        (if (pathname-name fp)
            (read-file env path)
            (notfound env)) ;; Open directory
        (notfound env)))))

(defun new-editable-file (env path)
  (make-temporary-package path)
  `(200 (:content-type "text/html")
    (,(render-template* +base.html+ nil
                        :root (directory-namestring path)
                        :host (getf env :server-name)
                        :port (getf env :server-port)
                        :path path
                        :plugins *plugin-scripts*
                        :token (write-to-string (get-universal-time))))))

(defun get-editable-file (env path)
  (make-temporary-package path)
  (with-open-file (in path :direction :input)
    (let ((editcells (read in)))
      (if (eq :darkmatter (car editcells))
        `(200 (:content-type "text/html")
          (,(render-template* +base.html+ nil
                              :editcells (cdr editcells)
                              :root (directory-namestring path)
                              :host (getf env :server-name)
                              :port (getf env :server-port)
                              :path path
                              :plugins *plugin-scripts*
                              :token (write-to-string (get-universal-time)))))
        (notfound env)))))


(defun notfound (env)
  `(404 (:content-type "text/plain") ("404 Not Found")))


