;;; test-json.el --- Flycheck Specs: JSON -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language JSON"
  (flycheck-buttercup-def-checker-test json-jsonlint json nil
    (flycheck-buttercup-should-syntax-check
     "language/json.json" 'json-mode
     '(1 44 error "found: ',' - expected: 'EOF'." :checker json-jsonlint)))

  (flycheck-buttercup-def-checker-test json-python-json json nil
    (let ((flycheck-disabled-checkers '(json-jsonlint)))
      (flycheck-buttercup-should-syntax-check
       "language/json.json" 'json-mode
       '(1 44 error "Extra data" :checker json-python-json))))

  (flycheck-buttercup-def-checker-test json-jq json nil
    (let ((flycheck-disabled-checkers '(json-jsonlint json-python-json)))
      (flycheck-buttercup-should-syntax-check
       "language/json.json" 'json-mode
       '(1 44 error "Expected value before ','" :checker json-jq)))))

;;; test-json.el ends here
