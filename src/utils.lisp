;;; utils.lisp
;;-*- coding:utf-8 -*-

;;; Copyright (c) Eddie.
;;; Distributed under the terms of the MIT License.

(in-package :cl-user)
(defpackage :darkmatter.utils
  (:use :cl)
  (:import-from :alexandria
                :starts-with-subseq)
  (:export :split
           :starts-case))
(in-package :darkmatter.utils)

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
