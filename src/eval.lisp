(in-package :cl-user)
(defpackage darkmatter.eval
  (:use :cl)
  (:import-from :alexandria
                :if-let)
  (:import-from :cl-markup
                :markup
                :escape-string)
  (:import-from :darkmatter.pacman
                :get-package
                :make-temporary-package)
  (:import-from :darkmatter.async
                :check-task
                :attach-runtask
                :attach-task-thread))
(in-package :darkmatter.eval)

(defmacro farguments (symbol)
  `(third (function-lambda-expression (symbol-function ,symbol))))

(defmacro marguments (symbol)
  `(third (car (last (function-lambda-expression (macro-function ,symbol))))))

(defun symbol-detail (symbol)
  (let ((name (symbol-name symbol)))
    (cond
      ((macro-function symbol)
       `(:obj ("type" . "macro")
              ("doc" . ,(documentation symbol 'function))
              ("arguments" . ,(marguments symbol))))
      ((fboundp symbol)
       `(:obj ("type" . "function")
              ("doc" . ,(documentation symbol 'function))
              ("arguments" . ,(farguments symbol))))
      ((find-class symbol nil)
       `(:obj ("type" . "class")
              ("doc" . ,(documentation symbol 'type))))
      ((boundp symbol)
       `(:obj ("type" . "variable")
              ("doc" . ,(documentation symbol 'variable))))
      (t
       `(:obj ("type" . "symbol"))))))

(defun eval-string (path src cell id)
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
         ($<error-output> "")
         ($<standard-output> "")
         (sexp nil)
         (symbols `(:obj))
         (return-value nil)
         (pos 0))
    (handler-case
      (loop while pos
            do (multiple-value-setq (sexp pos)
                 (read-from-string src :eof-error-p t :start pos))
               (setf sexp (attach-runtask sexp))
               (setf return-value (eval sexp))
               (when (symbolp return-value)
                 (setf symbols
                       (append symbols
                               (list
                                (cons (symbol-name return-value)
                                      (symbol-detail return-value)))))))
      (END-OF-FILE (c) nil)
      (error (c) (format t "<pre>~A</pre>" c)))
    (setf $<error-output> (get-output-stream-string *error-output*))
    (setf $<standard-output> (get-output-stream-string *standard-output*))
    (setf (cdr (gethash path darkmatter.pacman:*local-packages*)) (package-name *package*))
    (in-package :darkmatter)
    (format standard-output "Result:~A~%~A~%" return-value $<standard-output>)
    (if-let (task (check-task cell id return-value))
            (progn
              (setf (jsown:val task "symbols") symbols)
              task)
            `(:obj ("message" . "result")
                   ("return" .
                    ,(escape-string (format nil "~A" return-value)))
                   ("symbols" . ,symbols)
                   ("output" .
                    ,(format nil "~A~A"
                       (if (string= "" $<error-output>)
                           ""
                           (markup (:pre $<error-output>)))
                       (string-left-trim '(#\Space #\Newline) $<standard-output>))))))))


