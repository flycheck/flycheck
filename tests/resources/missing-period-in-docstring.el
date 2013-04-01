;;; missing-period-in-docstring -- Trigger checkdoc warning -*- lexical-binding: t; -*-

;;; Commentary:

;; Trigger a checkdoc warning by omitting a period in the docstring of a
;; function.  All these comments are to make checkdoc happy, which also checks
;; the general commenting style of an Emacs Lisp file.

;;; Code:

(defun foobar ()
  "Does nothing really useful")

;; The following is also to make checkdoc happy.
(provide 'missing-period-in-docstring)

;;; missing-period-in-docstring.el ends here
