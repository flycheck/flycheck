;;; test-css.el --- Flycheck Specs: CSS -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language CSS"
  (flycheck-buttercup-def-checker-test css-csslint css nil
    (flycheck-buttercup-should-syntax-check
     "language/css/warning.css" 'css-mode
     '(3 6 warning "Heading (h1) should not be qualified."
         :id "Disallowqualifiedheadings" :checker css-csslint)))

  (flycheck-buttercup-def-checker-test css-csslint css syntax-error
    (flycheck-buttercup-should-syntax-check
     "language/css/syntax-error.css" 'css-mode
     '(4 14 error "Expected a `FUNCTION` or `IDENT` after colon at line 4, col 14."
         :id "ParsingErrors" :checker css-csslint))))

;;; test-css.el ends here
