;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base 10 -*-

(defpackage #:cow-system
  (:use #:common-lisp #:asdf))

(in-package #:cow-system)

(defsystem cow
  ;; (Optional items omitted)
  :serial t ; the dependencies are linear
  :components ((:file "packages")
	       (:file "legs")
	       (:file "tail")
	       (:file "head")))

#|
;;; More complex example
(defsystem cow
  ;; (Optional items omitted)
  :components ((:file "tail"
		:depends-on ("packages" "legs"))
	       (:file "legs"
		:depends-on ("packages"))
	       (:file "head"
		:depends-on ("packages"))
	       (:file "packages")))
|#
