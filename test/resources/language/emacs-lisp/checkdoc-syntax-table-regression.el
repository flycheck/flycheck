;;; checkdoc-syntax-table-regression.el -- This file will confuse an improperly configured checkdoc -*- lexical-binding: t; -*-

;;; Commentary:

;; Checkdoc uses the syntax table of the current buffer; if not set properly, it
;; gets confused by the code below.

;;; Code:

(defun confusing ()
  ?{)

(provide 'checkdoc-syntax-table-regression)
;;; checkdoc-syntax-table-regression.el ends here
