;;; narrowing.el --- Trigger errors with narrowing

;;; Commentary:

;; A file to test narrowing

;;; Code:

(message "We are before of narrowing here: %s")

(defun we-narrow-to-this-function ()
  "This docstring has no final period"
  (message "Hello %s"))

(message "We are after of narrowing here: %s")

;;; narrowing.el ends here
