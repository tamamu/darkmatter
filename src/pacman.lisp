(in-package :cl-user)
(defpackage darkmatter.pacman
  (:use :cl)
  (:import-from :alexandria
                :ensure-symbol)
  (:export :*local-packages*
           :get-package
           :make-temporary-package
           :recall-package))
(in-package :darkmatter.pacman)

(defparameter *local-packages* (make-hash-table :test 'equal))

(defun get-package (path)
  (gethash path *local-packages*))

(defun (setf get-package) (val path)
  (setf (cdr (gethash path *local-packages*))
        (if (packagep val)
            (package-name val)
            val)))

;;; (<package> . "package-name")
(defun make-temporary-package (path)
  (print (directory-namestring path))
  (car (or (gethash path *local-packages*)
           (let* ((magic (write-to-string (get-universal-time)))
                  (pkg (make-package (format nil "darkmatter.local.~A" magic)
                                     :use `(:cl :darkmatter.infix :darkmatter.suite))))
             (setf (symbol-value (ensure-symbol :*current-directory* pkg))
                   (pathname
                    (directory-namestring
                     (merge-pathnames *default-pathname-defaults* path))))
             (use-package pkg 'darkmatter)
             (setf (gethash path *local-packages*)
                   (cons pkg (package-name pkg)))))))

(defun recall-package (path)
  (let ((pkg (car (get-package path))))
    (setf (gethash path *local-packages*) nil)
    (unuse-package pkg 'darkmatter)
    (delete-package pkg)
    (make-temporary-package path)
    '(:obj)))
