;;; test-handlebars.el --- Flycheck Specs: Handlebars -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Handlebars"
  (flycheck-buttercup-def-checker-test handlebars handlebars nil
    (flycheck-buttercup-should-syntax-check
     "language/handlebars.hbs" '(handlebars-mode web-mode)
     '(2 nil error "Expecting 'ID', 'STRING', 'NUMBER', 'BOOLEAN', 'UNDEFINED', 'NULL', 'DATA', got 'INVALID'"
         :checker handlebars)))

  (describe "reading the tool's output"
    ;; Read from output recorded earlier, so this runs whether or
    ;; not the tool is installed here

    (flycheck-buttercup-def-parse-test handlebars "language/handlebars.hbs"
      '(2 nil error "Expecting 'ID', 'STRING', 'NUMBER', 'BOOLEAN', 'UNDEFINED', 'NULL', 'DATA', got 'INVALID'"))))

;;; test-handlebars.el ends here
