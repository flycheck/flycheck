;;; test-eruby.el --- Flycheck Specs: eRuby -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language eRuby"
  (flycheck-buttercup-def-checker-test eruby-ruumba eruby syntax-error
    (flycheck-buttercup-should-syntax-check
     "language/eruby.erb" '(html-erb-mode rhtml-mode)
     '(8 1 error "unexpected token $end (Using Ruby 2.4 parser; configure using `TargetRubyVersion` parameter, under `AllCops`)"
         :id "Lint/Syntax" :checker eruby-ruumba))))

;;; test-eruby.el ends here
