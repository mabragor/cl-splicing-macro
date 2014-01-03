;;;; cl-splicing-macro.lisp

(in-package #:cl-splicing-macro)

(defmacro sprogn (&rest forms)
  (declare (ignore forms))
  (error "This macro is not meant to be expanded directly"))

(defvar *splicing-macro* (make-hash-table :test #'eq))

(defun mk-splicing-macroexpand-hook (old-macroexpand-hook)
  (lambda (fun form lexenv)
    (format t "I'm a loud macroexpand hook!~%")
    (funcall old-macroexpand-hook fun form lexenv)))


(let (installed)
  (defun enable-splicing-macro ()
    (if (not installed)
	(progn (setf installed *macroexpand-hook*)
	       (setf *macroexpand-hook* (mk-splicing-macroexpand-hook installed))
	       t)
	(format t "Splicing macro already enabled, doing nothing.")))
  (defun disable-splicing-macro ()
    (if installed
	(progn (setf *macroexpand-hook* installed)
	       (setf installed nil)
	       t)
	(format t "Splicing macro already disabled, doing nothing."))))

