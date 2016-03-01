;;; checkdoc-elisp-mode-regression.el -- This file will confuse an improperly configured checkdoc -*- lexical-binding: t; -*-

;;; Commentary:

;; Checkdoc uses the syntax table and comment syntax of the current buffer; if
;; not set properly, it gets confused by the code below.

;;; Code:

(defun confusing-syntax ()
  ?{)

(defconst confusing-docstring
  ;; comment "a"
  ".")

(provide 'checkdoc-elisp-mode-regression)
;;; checkdoc-elisp-mode-regression.el ends here
