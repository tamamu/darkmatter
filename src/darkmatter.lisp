(in-package :cl-user)
(defpackage darkmatter
  (:use :cl :websocket-driver)
  (:import-from :clack
                :clackup)
  (:import-from :lack.builder
                :builder)
  (:import-from :djula
                :add-template-directory
                :compile-template*
                :render-template*)
  (:import-from :string-case
                :string-case)
  (:import-from :cl-markup
                :markup
                :escape-string)
  (:import-from :alexandria
                :if-let
                :read-file-into-string
                :starts-with-subseq)
  (:export :start :stop :*eval-server*))
(in-package :darkmatter)

(defparameter *appfile-path*
  (asdf:system-relative-pathname "darkmatter" #P"app.lisp"))

(defvar *handler* nil)

(defparameter *root-directory*
  (asdf:system-relative-pathname "darkmatter" ""))

(defparameter *static-directory*
  (asdf:system-relative-pathname "darkmatter" "static/"))

(djula:add-template-directory (asdf:system-relative-pathname "darkmatter" "templates/"))
(defparameter +base.html+ (djula:compile-template* "base.html"))

(defparameter *local-packages* (make-hash-table :test 'equal))
(defparameter *package-temp* nil)

(defpackage darkmatter.plot
  (:use :cl)
  (:export scatter
           make-scatter
           line
           make-line))
(in-package :darkmatter.plot)
(defstruct scatter
  (xlabel "x" :type string)
  (ylabel "y" :type string)
  (data #() :type array))
(defstruct line
  (data #() :type array))
(in-package darkmatter)

(defmacro get-package (path)
  `(gethash ,path *local-packages*))

;;; (<package> . "package-name")
(defun make-temporary-package (path)
  (if-let (pkg (gethash path *local-packages*))
          (car pkg)
          (let* ((magic (write-to-string (get-universal-time)))
                 (pkg (make-package (format nil "darkmatter.local.~A" magic)
                                    :use `(:cl :darkmatter.plot :darkmatter.infix :darkmatter.suite))))
            (eval `(in-package ,(package-name pkg)))
            (defparameter *last-package* nil)
            (export *last-package*)
            (setf *default-pathname-defaults* (pathname (directory-namestring path)))
            (in-package :darkmatter)
            (use-package pkg 'darkmatter)
            (setf (gethash path *local-packages*)
                  (cons pkg (package-name pkg)))
            pkg)))

(defun recall-package (path)
  (let ((pkg (car (get-package path))))
    (setf (gethash path *local-packages*) nil)
    (unuse-package pkg 'darkmatter)
    (delete-package pkg)
    (make-temporary-package path)
    "{}"))

(defun eval-string (path src)
  (format t "Come: ~A~%" src)
  (let ((pkg (get-package path)))
    (when (null pkg)
      (let ((new-package (make-temporary-package path)))
        (setf pkg (cons new-package (package-name new-package)))))
    (if-let (last-package (cdr pkg))
            (eval `(in-package ,last-package))
            (eval `(in-package ,(package-name (car pkg)))))
  (let* ((standard-output *standard-output*)
         (*standard-output* (make-string-output-stream))
         (*error-output* (make-string-output-stream))
         (eo "")
         (so "")
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
    (setf so (get-output-stream-string *standard-output*))
;    (setf eo "")
    (setf (cdr (gethash path darkmatter::*local-packages*)) (package-name *package*))
    (in-package :darkmatter)
    (format standard-output "Result:~A~%~A~%" return-value so)
    (jsown:to-json
      `(:obj ("return" . ,(escape-string (format nil "~A" return-value)))
             ("output" . ,(format nil "~A~A"
                            (if (string= "" eo)
                                ""
                                (markup (:pre eo)))
                            (string-left-trim '(#\Space #\Newline) so))))))))

(defun save-file (fname data)
  (format t "save~%")
  (let ((res (list)))
    (loop for d in data
          for c = `((:id . ,(jsown:val d "id"))
                    (:next . ,(jsown:val d "next"))
                    (:prev . ,(jsown:val d "prev"))
                    (:count . ,(jsown:val d "count"))
                    (:lang . ,(jsown:val d "lang"))
                    (:lisp . ,(jsown:val d "lisp"))
                    (:md . ,(jsown:val d "md"))
                    (:output . ,(jsown:val d "output")))
          do (setf res (append res (list c))))
    (print (length res))
    (push :darkmatter res)
    (let ((path fname))
      (unless (ensure-directories-exist fname)
          (setf path "./tmp.dm.lisp"))
      (with-open-file (out path :direction :output :if-exists :supersede)
        (print res out))
      (jsown:to-json
        `(:obj ("return" . ,(format nil "~A" fname)))))))

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
          (get-editable-file path env)
          (new-editable-file path env))
      (if fp
        (if (pathname-name fp)
            (read-file env path)
            (notfound env)) ;; Open directory
        (notfound env)))))

(defun new-editable-file (path env)
  (make-temporary-package path)
  `(200 (:content-type "text/html")
    (,(render-template* +base.html+ nil
                        :root (directory-namestring path)
                        :host (getf env :server-name)
                        :port (getf env :server-port)
                        :path path))))

(defun get-editable-file (path env)
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
                              :path path)))
        (notfound env)))))

(defun notfound (env)
  `(404 (:content-type "text/plain") ("404 Not Found")))

(defun websocket-p (env)
  (string= "websocket" (gethash "upgrade" (getf env :headers))))

(defparameter *websocket-binder*
  (lambda (app bind)
    (lambda (env)
      (if (websocket-p env)
        (funcall bind env)
        (funcall app env))))
  "Middleware for binding websocket message")

(defun bind-message (env)
  "Bind websocket messages"
  (let ((ws (make-server env)))
    (on :message ws
        (lambda (message)
          (let* ((json (jsown:parse message))
                 (message (jsown:val json "message"))
                 (res (string-case
                        (message)
                        ("eval" (eval-string (jsown:val json "file")
                                             (jsown:val json "data")))
                        ("save" (save-file (jsown:val json "file")
                                           (jsown:val json "data")))
                        ("recall" (recall-package (jsown:val json "file")))
                        (t "{}"))))
            (send ws res))))
    (on :open ws
        (lambda ()
          (format t "Connected.~%")))
    (on :error ws
        (lambda (error)
          (format t "Got an error:~S~%" error)))
    (on :close ws
        (lambda (code reason)
          (format t "Closed because '~A' (Code=~A)~%" reason code)
          (start-connection ws)))
    (lambda (responder)
      (declare (ignore responder))
      (start-connection ws))))

(defun handle-get (env)
  (let ((uri (getf env :request-uri)))
    (if (string= "/" uri)
        (serve-index)
        (let ((path (subseq uri 1)))
          (if-let (data (read-global-file env path))
                  data
                  (if (string= "LISP" (string-upcase (pathname-type path)))
                      (get-editable-file path env)
                      (notfound env)))))))

(defun handle-put (env)
  (let ((input (flexi-streams:make-flexi-stream
                 (getf env :raw-body)
                 :external-format (flexi-streams:make-external-format :utf-8)))
        (recv (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
    (with-output-to-string (s recv)
      (loop for line = (read-line input nil nil) while line
            do (format s "~A~%" line)))
    (format t "~%~A~%" recv)
    (format t "---------------~%")
    (let* ((json (jsown:parse recv))
           (message (jsown:val json "message"))
           (res (string-case
                  (message)
                  ("eval" (eval-string (jsown:val json "file")
                                       (jsown:val json "data")))
                  ("save" (save-file (jsown:val json "file")
                                     (jsown:val json "data")))
                  ("recall" (recall-package (jsown:val json "file")))
                  (t "{}"))))
      `(201 (:content-type "application/json") (,res)))))

(defparameter *eval-server*
  (lambda (env)
    (if (eq :GET (getf env :request-method))
      (handle-get env)
      (handle-put env)))
  "File server")

(setf *eval-server*
(builder
  (:static :path (lambda (path)
                   (if (or (starts-with-subseq "/static/" path)
                           (starts-with-subseq "/bower_components/" path))
                     path
                     nil))
           :root *root-directory*)
  *eval-server*))

(setf *eval-server*
      (funcall *websocket-binder* *eval-server* #'bind-message))

(defun start (&rest args &key server port &allow-other-keys)
  (declare (ignore server port))
  (when *handler*
    (restart-case (error "Darkmatter is already running.")
      (restart-darkmatter ()
        :report "Restart Darkmatter"
        (stop))))
  (setf *handler*
        (apply #'clackup *appfile-path* args)))

(defun stop ()
  (prog1
    (clack:stop *handler*)
    (setf *handler* nil)))
