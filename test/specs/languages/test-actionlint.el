;;; test-actionlint.el --- Flycheck Specs: Actionlint -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language YAML (actionlint)"
  (flycheck-buttercup-def-checker-test yaml-actionlint yaml nil
    (flycheck-buttercup-should-syntax-check
     "language/.github/workflows/test-action.yml" 'yaml-mode
     '(6 23 error "property \"foo\" is not defined in object type {}"
         :id "expression" :end-line 6 :end-column 33
         :checker yaml-actionlint)))

  (describe "reading the tool's output"
    ;; Read from output recorded earlier, so this runs whether or
    ;; not the tool is installed here

    (flycheck-buttercup-def-parse-test yaml-actionlint "language/.github/workflows/test-action.yml"
      ;; The JSON carries an inclusive end column the oneline format lacked
      '(6 23 error "property \"foo\" is not defined in object type {}"
          :id "expression" :end-line 6 :end-column 33))))

;;; test-actionlint.el ends here
