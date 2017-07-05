(in-package :cl-user)
(defpackage darkmatter.plot
  (:use :cl)
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


