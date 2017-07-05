(in-package :cl-user)
(defpackage darkmatter
  (:use :cl :websocket-driver :bordeaux-threads)
  (:import-from :clack
                :clackup)
  (:import-from :lack.builder
                :builder)
  (:import-from :string-case
                :string-case)
  (:import-from :alexandria
                :if-let
                :starts-with-subseq)
  (:import-from :darkmatter.async
                :attach-task-thread
                :get-task-output
                :get-task-thread
                :send-recv
                :exists-task)
  (:import-from :darkmatter.serve
                :read-global-file
                :serve-index
                :get-editable-file
                :notfound)
  (:import-from :darkmatter.eval
                :eval-string)
  (:import-from :darkmatter.pacman
                :recall-package)
  (:import-from :darkmatter.save
                :save-file)
  (:import-from :darkmatter.plugman
                :get-plugin)
  (:export :start :stop :*eval-server*))
(in-package :darkmatter)
(defparameter *appfile-path*
  (asdf:system-relative-pathname "darkmatter" #P"app.lisp"))

(defvar *handler* nil)

(defparameter *root-directory*
  (asdf:system-relative-pathname "darkmatter" ""))

(defun websocket-p (env)
  (string= "websocket" (gethash "upgrade" (getf env :headers))))

(defparameter *websocket-binder*
  (lambda (app bind)
    (lambda (env)
      (if (websocket-p env)
        (funcall bind env)
        (funcall app env))))
  "Middleware for binding websocket message")

(defun bind-init (ws id)
  (attach-task-thread id)
  (send ws
        (jsown:to-json
          `(:obj ("message" . "init")
                 ("id" . ,id)
                 ("output" . (get-task-output id))))))

(defun bind-kill (ws id)
  (if-let (task (exists-task id))
          (progn
            (set-task-kill id)
            (if-let (thread (get-task-thread id))
                    (join-thread thread))))
  (send-recv ws id))

(defun bind-message (env)
  "Bind websocket messages"
  (let ((ws (make-server env))
        (addr (getf env :remote-addr))
        (port (write-to-string (getf env :remote-port))))
    (on :message ws
        (lambda (message)
          (let* ((json (jsown:parse message))
                 (message (jsown:val json "message"))
                 (id (jsown:val json "id")))
            (string-case (message)
              ("init" (bind-init ws id))
              ("recv" (send-recv ws id))
              ("kill" (bind-kill ws id))
              (t (send ws (jsown:to-json '(:obj ("message" . "none")))))))))
    (on :open ws
        (lambda ()
          (format t "Connected.~%")))
    (on :error ws
        (lambda (error)
          (format t "Got an error:~S~%" error)))
    (on :close ws
        (lambda (code reason)
          (format t "Closed because '~A' (Code=~A)~%" reason code)))
    (lambda (responder)
      (declare (ignore responder))
      (start-connection ws))))

(defun handle-get (env)
  (let ((uri (getf env :request-uri)))
    (if (string= "/" uri)
        (serve-index)
        (let ((path (subseq uri 1)))
          (multiple-value-bind (browse-p browse-path)
            (starts-with-subseq "browse/" path :return-suffix t)
            (if (and browse-p
                     (not (starts-with-subseq "/" browse-path)))
                (if-let (data (read-global-file env browse-path))
                        data
                        (if (string= "LISP" (string-upcase (pathname-type path)))
                            (get-editable-file env browse-path)
                            (notfound env)))
                (multiple-value-bind (plugin-p plugin-path)
                  (starts-with-subseq "plugin/" path :return-suffix t)
                  (format t "~A:~A~%" plugin-p plugin-path)
                  (if plugin-p
                      (get-plugin env plugin-path)
                      (notfound env)))))))))

(defun handle-put (env)
  (let ((input (flexi-streams:make-flexi-stream
                 (getf env :raw-body)
                 :external-format (flexi-streams:make-external-format :utf-8)))
        (recv (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))
        (addr (getf env :remote-addr))
        (port (write-to-string (getf env :remote-port))))
    (with-output-to-string (s recv)
      (loop for line = (read-line input nil nil) while line
            do (format s "~A~%" line)))
    (let* ((json (jsown:parse recv))
           (message (jsown:val json "message"))
           (cell (jsown:val json "cell"))
           (token (jsown:val json "token"))
           (file (jsown:val json "file"))
           (id (concatenate 'string addr
                                ":" port
                                ":" token
                                ":" file))
           (res (string-case
                  (message)
                  ("eval" (eval-string file
                                       (jsown:val json "data")
                                       cell
                                       id))
                  ("save" (save-file file
                                     (jsown:val json "data")))
                  ("recall" (recall-package file))
                  (t "{}"))))
      `(201 (:content-type "application/json") (,(jsown:to-json res))))))

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
