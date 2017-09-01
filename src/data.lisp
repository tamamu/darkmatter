;;; data.lisp
;;-*- coding:utf-8 -*-

;;; Copyright (c) Eddie.
;;; Distributed under the terms of the MIT License.

(in-package :cl-user)
(defpackage darkmatter.data
  (:use :cl)
  (:export :load-notebook
           :save-as-ipynb))
(in-package :darkmatter.data)

(defstruct ipynb-format
  (kernel-info nil :type list)
  (language-info nil :type list)
  (nbformat 4 :type integer)
  (nbformat-minor 0 :type integer)
  (cells nil :type list))

(define-condition ipynb-parse-error (simple-error) (field)
  (:report
    (lambda (c s)
      (format s "ipynb parse error: ~A"
              (ipynb-parse-error-field c)))))

(defun read-string-from-file (path)
  (with-open-file (s path :direction :input)
    (let ((buf (make-string (file-length s))))
      (read-sequence buf s)
      buf)))

(defun ipynb-hash-bind (ht)
  (let ((metadata (gethash "metadata" ht)))
    (if (null metadata)
        (error (make-condition 'ipynb-parse-error :field "metadata"))
        (let ((kernel-info (gethash "kernel_info" metadata))
              (language-info (gethash "language_info" metadata))
              (nbformat (gethash "nbformat" ht))
              (nbformat-minor (gethash "nbformat_minor" ht))
              (cells (gethash "cells" ht)))
          (cond
            ((not (integerp nbformat))
             (error (make-condition 'ipynb-parse-error :field "nbformat")))
            ((not (integerp nbformat-minor))
             (error (make-condition 'ipynb-parse-error :field "nbformat-minor")))
            ((not (listp cells))
             (error (make-condition 'ipynb-parse-error :field "cells")))
            (t (values kernel-info language-info nbformat
                       nbformat-minor cells)))))))

(defun parse-cell (ht)
  (let* ((cell-type (gethash "cell_type" ht))
         (metadata (gethash "metadata" ht))
         (source (gethash "source"))
         (source-string (if (listp source)
                            (format nil "~{~A~}" source)
                            source)))
    (list :cell-type cell-type
          :metadata nil
          :source source-string)))

(defun load-ipynb (env path)
  (let ((fp (probe-file path)))
    (if (null fp)
        (new-notebook env path)
        (let ((json (yason:parse (read-string-from-file fp))))
          (multiple-value-bind
            (kernel-info language-info nbformat nbformat-minor cells)
            (ipynb-hash-bind json)

            (mapcar #'parse-cell cells))))))

(defun save-as-ipynb (env path raw))
