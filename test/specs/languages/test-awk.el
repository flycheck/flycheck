;;; test-awk.el --- Flycheck Specs: AWK -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language AWK"
  (flycheck-buttercup-def-checker-test awk-gawk awk syntax-error
    (flycheck-buttercup-should-syntax-check
     "language/awk/syntax-error.awk" 'awk-mode
     '(2 nil warning "x=|\n  ^ syntax error" :checker awk-gawk)))

  (flycheck-buttercup-def-checker-test awk-gawk awk valid
    ;; A valid script must not be flagged just because gawk's lint
    ;; harness exits non-zero.  See URL
    ;; `https://github.com/flycheck/flycheck/issues/2166'.
    (flycheck-buttercup-should-syntax-check
     "language/awk/valid.awk" 'awk-mode)))

;;; test-awk.el ends here
