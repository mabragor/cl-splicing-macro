;;;; package.lisp

(defpackage #:cl-splicing-macro
  (:use #:cl)
  (:export #:sprogn #:define-maybe-splicing-macro #:define-maybe-sampling-macro
	   #:enable-splicing-macro #:disable-splicing-macro
	   #:with-splicing-macro-runtime))

