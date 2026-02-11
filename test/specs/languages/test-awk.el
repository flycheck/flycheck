;;; test-awk.el --- Flycheck Specs: AWK -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language AWK"
  (flycheck-buttercup-def-checker-test awk-gawk awk syntax-error
    (flycheck-buttercup-should-syntax-check
     "language/awk/syntax-error.awk" 'awk-mode
     '(2 nil warning "x=|\n  ^ syntax error" :checker awk-gawk))))

;;; test-awk.el ends here
