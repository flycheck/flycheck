;;; test-org.el --- Flycheck Specs: Org -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Org"
  (flycheck-buttercup-def-checker-test org-lint org nil
    (let ((inhibit-message t))
      (flycheck-buttercup-should-syntax-check
       "language/org/lint.org" 'org-mode
       '(21 nil info "Link to non-existent local file \"nonexistent.org\"" :checker org-lint)
       '(24 nil info "Link to non-existent local file \"/tmp/does-not-exist.org\"" :checker org-lint)
       '(28 nil info "Duplicate CUSTOM_ID property \"duplicate-id-123\"" :checker org-lint)
       '(33 nil info "Duplicate CUSTOM_ID property \"duplicate-id-123\"" :checker org-lint)
       '(40 nil info "Link to non-existent local file \"file with spaces and \\\"quotes\\\".org\"" :checker org-lint))))

  (flycheck-buttercup-def-checker-test org-lint-valid org nil
    (let ((inhibit-message t))
      (flycheck-buttercup-should-syntax-check
       "language/org/valid.org" 'org-mode))))

;;; test-org.el ends here
