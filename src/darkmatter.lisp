(in-package :cl-user)
(defpackage darkmatter
  (:use :cl :websocket-driver)
  (:export *eval-server*))
(in-package :darkmatter)

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
      `(:obj ("return" . ,return-value)
             ("output" . ,(format nil "~A~A"
                            (if (string= "" eo)
                                ""
                                (format nil "<pre>~A</pre>" eo))
                            (string-left-trim '(#\Space #\Newline)
                              (get-output-stream-string *standard-output*))))))))

(defvar *eval-server*
  (lambda (env)
    (let ((ws (make-server env)))
      (on :message ws
        (lambda (message)
          (let ((res (eval-string message)))
            (send ws res))))
    (lambda (responder)
      (declare (ignore responder))
      (start-connection ws)))))
