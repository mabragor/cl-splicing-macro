
(in-package #:sb-c)

(def-ir1-translator with-splicing-macro ((&body body) start next result)
  (unwind-protect (progn (cl-splicing-macro::%enable-splicing-macro)
			 (ir1-convert-progn-body start next result body))
    (cl-splicing-macro::%disable-splicing-macro)))

(export '(with-splicing-macro))

