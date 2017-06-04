(in-package :cl)
(defpackage darkmatter.infix
  (:use :cl)
  (:export :*operator-order*
           :enable-infix-syntax
           :disable-infix-syntax
           :with-infix))
(in-package :darkmatter.infix)

(defparameter *operator-order*
  '(* /))
(defparameter *previous-readtables* nil)

(defun operator> (op1 op2)
  "The value of operator> is true if op1 is a higher priority than op2; otherwise it is false"
  (let ((p1 (position op1 *operator-order*))
        (p2 (position op2 *operator-order*)))
    (if (null p1)
        nil
        (if (null p2)
            t
            (> p1 p2)))))

(defun formulap (expr)
  (and (consp expr) (not (fboundp (car expr)))))

(defun transform-formula (body)
  "Transform infix formula to prefix formula"
  (let ((left (pop body)))
    (when (formulap left)
          (setq left (transform-formula left)))
    (loop for op1 = (pop body)
          for right1 = (pop body)
          when (formulap right1)
          do (setq right1 (transform-formula right1))
          until (null op1)
          do (loop for op2 = (car body)
                   until (null op2)
                   if (operator> op2 op1)
                   do (setq op2 (pop body))
                      (let ((right2 (pop body)))
                        (when (formulap right2)
                              (setq right2 (transform-formula right2)))
                        (setq right1 (list op2 right1 right2)))
                   else
                   do (return))
          do (setq left (list op1 left right1)))
    left))

(defmacro enable-infix-syntax ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
     (push *readtable* *previous-readtables*)
     (setq *readtable* (copy-readtable))
     (set-dispatch-macro-character #\# #\f
       (lambda (s c n)
         (declare (ignore c n))
         (let ((list (read s nil (values) t)))
           (transform-formula list))))))

(defmacro disable-infix-syntax ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
     (setq *readtable* (pop *previous-readtables*))))

(defmacro with-infix (&body body)
  `(progn
     (enable-infix-syntax)
     ,@body
     (disable-infix-syntax)))
