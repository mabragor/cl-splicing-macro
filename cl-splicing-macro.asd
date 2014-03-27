;;;; cl-splicing-macro.asd

(asdf:defsystem #:cl-splicing-macro
  :version "0.1"
  :serial t
  :description "Deforms macros in a way, that they can capture surrounding s-exps for their expansion."
  :author "Alexandr Popolitov <popolit@gmail.com>"
  :license "GPL"
  :components ((:file "package")
	       #+sbcl (:file "sbcl")
               (:file "cl-splicing-macro")
	       #+sbcl (:file "sbcl-2")
	       ))
	       

(defsystem :cl-splicing-macro-tests
  :description "Tests for CL-SPLICING-MACRO."
  :licence "GPL"
  :depends-on (:cl-splicing-macro :fiveam)
  :components ((:file "tests")))

(defmethod perform ((op test-op) (sys (eql (find-system :cl-splicing-macro))))
  (load-system :cl-splicing-macro-tests)
  (funcall (intern "RUN-TESTS" :cl-splicing-macro-tests)))
