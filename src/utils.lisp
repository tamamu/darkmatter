;;; utils.lisp
;;-*- coding:utf-8 -*-

;;; Copyright (c) Eddie.
;;; Distributed under the terms of the MIT License.

(in-package :cl-user)
(defpackage :darkmatter.utils
  (:use :cl)
  (:import-from :alexandria
                :starts-with-subseq)
  (:export :gen-serial
           :split
           :starts-case
           :escape
           :%log))
(in-package :darkmatter.utils)

(defun gen-serial (start)
  (let ((num start))
  (lambda () (incf num))))

(defun split (str delim)
  (let ((res (make-array 0 :element-type 'string
                           :fill-pointer 0
                           :adjustable t)))
    (loop for i from 0 below (length str)
          with start = 0
          when (eq (char str i) delim)
          do (vector-push-extend (subseq str start i) res)
             (setf start (1+ i))
          finally (let ((tail (subseq str start)))
                    (when tail
                      (vector-push-extend tail res))))
    res))

(defun starts-case (keyform cases)
  "Call the function with subsequence if the given string starts with pattern within cases."
  (dolist (case cases)
    (destructuring-bind (pattern matched) case
      (if (eq pattern 'otherwise)
          (return (funcall matched keyform))
          (multiple-value-bind (match-p remain)
            (starts-with-subseq pattern keyform :return-suffix t)
            (when match-p (return (funcall matched remain))))))))

(defun escape (str)
  (let ((stream (make-string-output-stream)))
    (loop for c across str
          do (case c
               (#\< (write-string "&lt;" stream))
               (#\> (write-string "&gt;" stream))
               (otherwise (write-char c stream))))
    (get-output-stream-string stream)))

(defun %log (message &optional type)
  (multiple-value-bind
    (second minute hour date month year day-of-week dst-p tz)
    (get-decoded-time)
    (declare (ignore date month year day-of-week dst-p tz))
    (format t "~C[32;1m[~2,'0d:~2,'0d:~2,'0d]~C[0m~A ~A~%"
            (code-char #o33) hour minute second (code-char #o33)
            (if (null type)
                ""
                (format nil " ~C[34;1m~A~C[0m"
                        (code-char #o33) type (code-char #o33)))
            message)
    (force-output)))
