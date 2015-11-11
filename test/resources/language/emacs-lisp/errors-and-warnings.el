;;; errors-and-warnings.el -- Trigger Emacs Lisp warnings -*- lexical-binding: t; -*-

;;; Commentary:

;; Trigger a checkdoc warning by omitting a period in the docstring of a
;; function.  All these comments are to make checkdoc happy, which also checks
;; the general commenting style of an Emacs Lisp file.

;;; Code:

(defun foobar ()
  "Does nothing really useful"
  (dummy-package-foo))

(require 'dummy-package)

(if t
    (message)
  (message "Hello foo"))

(provide 'errors-and-warnings)
;;; errors-and-warnings.el ends here
