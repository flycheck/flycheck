;;; many-errors.el --- Trigger many errors

;;; Commentary:

;; A file with many errors to test error navigation.

(message "Hello %s")

(princ)

(i-do-not-exist)

;;; many-errors.el ends here
