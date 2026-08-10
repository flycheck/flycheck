;;; test-actionlint.el --- Flycheck Specs: Actionlint -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language YAML (actionlint)"
  (flycheck-buttercup-def-checker-test yaml-actionlint yaml nil
    (flycheck-buttercup-should-syntax-check
     "language/.github/workflows/test-action.yml" 'yaml-mode
     '(6 23 error "property \"foo\" is not defined in object type {}"
         :id "expression" :checker yaml-actionlint)))

  (describe "reading the tool's output"
    ;; Read from output recorded earlier, so this runs whether or
    ;; not the tool is installed here

    (flycheck-buttercup-def-parse-test yaml-actionlint "language/.github/workflows/test-action.yml"
      '(6 23 error "property \"foo\" is not defined in object type {}"
          :id "expression"))))

;;; test-actionlint.el ends here
