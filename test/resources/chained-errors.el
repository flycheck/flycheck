;;; chained-errors.el --- A file with errors from two chained checkers

;;; Commentary:

;; A file with errors from two chained checkers to test that chaining preserves
;; all errors.

(message "Hello %s")

(princ)

(i-do-not-exist)

;;; chained-errors.el ends here
