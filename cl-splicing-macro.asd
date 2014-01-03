;;;; cl-splicing-macro.asd

(asdf:defsystem #:cl-splicing-macro
  :serial t
  :description "Deforms macros in a way, that they can capture surrounding s-exps for their expansion."
  :author "Alexandr Popolitov <popolit@gmail.com>"
  :license "GPL"
  :components ((:file "package")
               (:file "cl-splicing-macro")))

