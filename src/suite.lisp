(in-package :cl)
(defpackage darkmatter.suite
  (:use :cl)
  (:export :->
           :$
           :!
           :enable-suite-syntax
           :disable-suite-syntax
           :with-suite))
(in-package :darkmatter.suite)

(defparameter *previous-readtables* nil)

(defmacro -> (arg &rest rest)
  (let ((result arg))
    (loop for f = (pop rest) while f
          do (setf result (list 'funcall f result)))
    result))

(defmacro $ (fun &rest args)
  `(lambda (x)
    (funcall ,fun ,@args x)))

(defmacro ! (body)
  (let ((args (list))
        (result (list)))
    (loop for s = (pop body) while s
          with cnt = 0
          with n = nil
          if (eq s '@)
          do (setf n (format nil "a~A" cnt)
                   s (intern n)
                   cnt (1+ cnt)
                   result (append result (list s))
                   args (append args (list s)))
          else
          do (setf result (append result (list s))))
    `(lambda (,@args) (,@result))))

(defmacro enable-suite-syntax ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
     (push *readtable* *previous-readtables*)
     (setq *readtable* (copy-readtable))
     (set-dispatch-macro-character #\# #\!
       (lambda (s c n)
         (declare (ignore c n))
         (let ((lst (read s nil (values) t)))
           (eval `(! ,lst)))))))

(defmacro disable-suite-syntax ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
     (setq *readtable* (pop *previous-readtables*))))

(defmacro with-suite (&body body)
  `(progn
     (enable-suite-syntax)
     ,@body
     (disable-suite-syntax)))
