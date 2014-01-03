;;;; package.lisp

(defpackage #:cl-splicing-macro
  (:use #:cl)
  (:export #:sprogn
	   #:enable-splicing-macro #:disable-splicing-macro))

