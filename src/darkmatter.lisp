(in-package :cl-user)
(defpackage darkmatter
  (:use :cl :websocket-driver)
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
  (:export *eval-server*))
(in-package :darkmatter)

(djula:add-template-directory (asdf:system-relative-pathname "darkmatter" "templates/"))
(defparameter +base.html+ (djula:compile-template* "base.html"))

(defpackage darkmatter.plot
  (:use :cl)
  (:export scatter
           make-scatter))
(in-package :darkmatter.plot)
(defstruct scatter
  (xlabel "x" :type string)
  (ylabel "y" :type string)
  (data #() :type array))
(in-package darkmatter)

(defpackage darkmatter.local
  (:use :cl)
  (:export *last-package*))
(in-package :darkmatter.local)
(defparameter *last-package* (package-name *package*))
(in-package :darkmatter)

(defun eval-string (src)
  (format t "Come: ~A~%" src)
  (eval `(in-package ,darkmatter.local:*last-package*))
  (let* ((*standard-output* (make-string-output-stream))
         (*error-output* (make-string-output-stream))
         (eo "")
         (sexp nil)
         (return-value nil)
         (pos 0))
    (handler-case
      (loop while pos
            do (multiple-value-setq (sexp pos)
                 (read-from-string src :eof-error-p t :start pos))
               (setf return-value (eval sexp)))
      (END-OF-FILE (c) nil)
      (error (c) (format t "<pre>~A</pre>" c)))
    (setf eo (get-output-stream-string *error-output*))
    (setf darkmatter.local:*last-package* (package-name *package*))
    (in-package :darkmatter)
    (jsown:to-json
      `(:obj ("return" . ,(format nil "~A" return-value))
             ("output" . ,(format nil "~A~A"
                            (if (string= "" eo)
                                ""
                                (format nil "<pre>~A</pre>" eo))
                            (string-left-trim '(#\Space #\Newline)
                              (get-output-stream-string *standard-output*))))))))

(defun save-file (fname src)
  (with-open-file (out fname :direction :output :if-exists :supersede)
    (format out "~A" src))
  (jsown:to-json
    `(:obj ("return" . ,(format nil "~A" fname)))))

(defun get-mime-type (path)
  (string-case ((string-upcase (pathname-type path)))
    ("HTML" "text/html")
    ("CSS" "text/css")
    ("JS" "text/javascript")
    ("JSON" "application/json")
    ("PNG" "image/png")
    ("JPEG" "image/jpeg")
    ("JPG" "image/jpeg")
    (t "text/plain")))

(defun read-static-file (path)
  (let ((p (asdf:system-relative-pathname "darkmatter" path)))
    (if (probe-file p)
        `(200 (:content-type ,(get-mime-type p))
          (,(read-file-into-string p)))
        nil)))

(defun read-global-file (path)
  (if-let (p (probe-file path))
    (if (pathname-name p)
        `(200 (:content-type ,(get-mime-type p))
          (,(read-file-into-string p)))
        `(200 (:content-type "text/html")
          ("directory")))
    nil))

(defun get-editable-file (path env)
  `(200 (:content-type "text/html")
    (,(render-template* +base.html+ nil
                    :host (getf env :server-name)
                    :port (getf env :server-port)
                    :path path))))

(defun notfound ()
  (read-static-file "static/404.html"))

(defvar *eval-server*
  (lambda (env)
    (if (string= "websocket" (gethash "upgrade" (getf env :headers)))
        (let ((ws (make-server env)))
          (on :message ws
              (lambda (message)
                (let* ((json (jsown:parse message))
                       (message (jsown:val json "message"))
                       (res
                         (string-case (message)
                           ("eval" (eval-string (jsown:val json "data")))
                           ("save" (save-file (jsown:val json "file")
                                              (jsown:val json "data")))
                           (t "{}"))))
                  (send ws res))))
          (lambda (responder)
            (declare (ignore responder))
            (start-connection ws)))
        (let ((uri (getf env :request-uri)))
          (if (string= "/" uri)
              (read-static-file "static/index.html")
              (let ((path (subseq uri 1)))
                (if (starts-with-subseq "//" uri :test #'string=)
                    (if-let (data (read-global-file path))
                            data
                            (if (string= "LISP" (string-upcase (pathname-type path)))
                                (get-editable-file path env)
                                (notfound)))
                    (if-let (data (read-static-file path))
                          data
                          (notfound)))))))))
