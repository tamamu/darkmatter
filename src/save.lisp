(in-package :cl-user)
(defpackage darkmatter.save
  (:use :cl)
  (:export :save-file))
(in-package :darkmatter.save)

(defun save-file (fname data)
  (format t "save~%")
  (let ((res (list)))
    (loop for d in data
          for c = `((:id . ,(jsown:val d "id"))
                    (:next . ,(jsown:val d "next"))
                    (:prev . ,(jsown:val d "prev"))
                    (:count . ,(jsown:val d "count"))
                    (:lang . ,(jsown:val d "lang"))
                    (:lisp . ,(jsown:val d "lisp"))
                    (:md . ,(jsown:val d "md"))
                    (:output . ,(jsown:val d "output")))
          do (setf res (append res (list c))))
    (push :darkmatter res)
    (let ((path fname))
      (unless (ensure-directories-exist fname)
          (setf path "./tmp.dm.lisp"))
      (with-open-file (out path :direction :output :if-exists :supersede)
        (print res out))
        `(:obj ("return" . ,(format nil "~A" fname))))))


