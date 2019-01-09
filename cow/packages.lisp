(defpackage #:tail
  (:use #:common-lisp #:legs))

(defpackage #:legs
  (:use #:common-lisp #:head))

(defpackage #:head
  (:use #:common-lisp))
