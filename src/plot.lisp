(in-package :darkmatter-user)
(defpackage darkmatter.plot
  (:use :cl :darkmatter.plugman)
  (:import-from :darkmatter.serve
                :*static-directory*
                :read-file)
  (:export scatter
           make-scatter
           line
           make-line))
(in-package :darkmatter.plot)

(defstruct scatter
  (xlabel "x" :type string)
  (ylabel "y" :type string)
  (data #() :type array))
(defstruct line
  (data #() :type array))

(defun handle (env path)
  (read-file env (merge-pathnames *static-directory* "LispPlot.js")))

(regist-plugin-handler "plot" #'handle)
(regist-plugin-script "plot" "LispPlot.js")
