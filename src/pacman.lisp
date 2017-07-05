(in-package :cl-user)
(defpackage darkmatter.pacman
  (:use :cl)
  (:import-from :alexandria
                :if-let
                :ensure-symbol)
  (:export :*local-packages*
           :get-package
           :make-temporary-package
           :recall-package))
(in-package :darkmatter.pacman)

(defparameter *local-packages* (make-hash-table :test 'equal))

(defmacro get-package (path)
  `(gethash ,path *local-packages*))

;;; (<package> . "package-name")
(defun make-temporary-package (path)
  (print (directory-namestring path))
  (if-let (pkg (gethash path *local-packages*))
          (car pkg)
          (let* ((magic (write-to-string (get-universal-time)))
                 (pkg (make-package (format nil "darkmatter.local.~A" magic)
                                  :use `(:cl :darkmatter.infix :darkmatter.suite))))
            (eval `(in-package ,(package-name pkg)))
            (setf (symbol-value (ensure-symbol :*current-directory* pkg))
                  (pathname
                    (directory-namestring
                      (merge-pathnames *default-pathname-defaults* path))))
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
    '(:obj)))


