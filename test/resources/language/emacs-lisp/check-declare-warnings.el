;;; check-declare-warnings.el -- check-declare warnings -*- lexical-binding: t; -*-

;;; Commentary:

;; Test check-declare warnings

;;; Code:

(declare-function this-function-is-not-declared "/this-file-does-not-exist.el")

(provide 'check-declare-warnings)
;;; check-declare-warnings.el ends here
