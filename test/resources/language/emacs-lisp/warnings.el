;;; warnings.el -- Trigger Emacs Lisp warnings -*- lexical-binding: t; -*-

;;; Commentary:

;; Trigger a checkdoc warning by omitting a period in the docstring of a
;; function.  All these comments are to make checkdoc happy, which also checks
;; the general commenting style of an Emacs Lisp file.

;;; Code:

(defun foobar ()
  "Does nothing really useful"
  (dummy-package-foo))

(if t
    (message)
  (message "Hello foo"))

(provide 'warnings)
;;; warnings.el ends here
