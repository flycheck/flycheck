;;; test-actionlint.el --- Flycheck Specs: Actionlint -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language YAML (actionlint)"
  (flycheck-buttercup-def-checker-test yaml-actionlint yaml nil
    (flycheck-buttercup-should-syntax-check
     "language/.github/workflows/test-action.yml" 'yaml-mode
     '(6 23 error "property \"foo\" is not defined in object type {} [expression]"
         :checker yaml-actionlint))))

;;; test-actionlint.el ends here
