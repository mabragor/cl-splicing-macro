;;;; cl-splicing-macro.lisp

(in-package #:cl-splicing-macro)

(defmacro sprogn (&rest forms)
  (declare (ignore forms))
  (error "This macro is not meant to be expanded directly"))

(defvar *splicing-macro* (make-hash-table))
(defvar *sample-expansion-data* (make-hash-table))

(defun mk-splicing-macroexpand-hook (old-macroexpand-hook)
  (lambda (fun form lexenv)
    ;; We need to treat compiler macro separately, cause they seem
    ;; to cause infinite macroexpansion loops otherwise
    (if (eq (compiler-macro-function (car form) lexenv)
	    fun)
	(funcall old-macroexpand-hook fun form lexenv)
	(funcall old-macroexpand-hook fun
		 (let (res)
		   (dolist (subform (cdr form) (cons (car form)
						     (reverse res)))
		     (if (and (listp subform)
			      (gethash (car subform) *splicing-macro*))
			 (dolist (elt (cdr (macroexpand-1 subform lexenv)))
			   (push elt res))
			 (push subform res))))
		 lexenv))))

(defun ampersand-symbol-p (sym)
  (and (symbolp sym)
       (not (keywordp sym))
       (char= #\& (char (string sym) 0))))

(defun parse-sample-from-lambda-list (lst)
  (let (res got-sample sample)
    (do ((cur lst (cdr cur)))
	((endp cur))
      (if (and (not got-sample)
	       (symbolp (car cur))
	       (string= "&SAMPLE" (string (car cur))))
	  (progn (setf cur (cdr cur))
		 (if (or (not cur) (ampersand-symbol-p (car cur)))
		     (error "Sample specification is missing after &SAMPLE magic word.")
		     (setf sample (car cur)
			   got-sample t))
		 (assert (listp sample)))
	  (push (car cur) res)))
    (values (nreverse res) sample got-sample)))

(defmacro define-maybe-sampling-macro (name lambda-list &rest body)
  (multiple-value-bind (residual-list sample got) (parse-sample-from-lambda-list lambda-list)
    `(progn (defmacro ,name ,residual-list ,@body)
	    ,(if got
		 `(setf (gethash ',name *sample-expansion-data*) 
			',sample)))))
		
(defmacro testing-expansion (name &environment env)
  (multiple-value-bind (sample-expansion-data got) (gethash name *sample-expansion-data*)
     (if (not got)
	 `(values nil nil)
	 `(values ',(macroexpand-1 `(,name ,@sample-expansion-data) env)
		  t))))

(defmacro define-maybe-splicing-macro (name lambda-list &rest body)
  `(progn (define-maybe-sampling-macro ,name ,lambda-list ,@body)
	  (multiple-value-bind (expansion has-sample-expansion) (testing-expansion ,name)
	    (if (and has-sample-expansion
		     (eq (car expansion) 'sprogn))
		(setf (gethash ',name *splicing-macro*) t)))))

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

;; OK, now that I have a basic skeleton of my project, time to
;; write something nontrivial!)

;; OK, I need to implement sample macroexpansion for MAYBE-SPLICING to work
