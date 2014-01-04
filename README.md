cl-splicing-macro
=================

This package adds a possibility for macros to inject several forms
(not just one) into surrounding macros's body.

```lisp
CL-SPLICING-MACRO> (defmacro bar (&rest things)
                     "Just a toy macros, that expands into a PLUS"
                     `(+ ,@things))
CL-SPLICING-MACRO> (define-maybe-splicing-macro foo (a b &sample (1 2))
                     "Just a toy macro that will splice in the body of surrounding macro."
                     `(sprogn ,a ,b))
CL-SPLICING-MACRO> (macroexpand-1 '(bar 1 2 (foo 3 4) 5 6))
(+ 1 2 3 4 5 6) ; enjoy!
```
