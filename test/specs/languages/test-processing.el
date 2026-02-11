;;; test-processing.el --- Flycheck Specs: Processing -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Processing"
  (flycheck-buttercup-def-checker-test processing processing syntax-error
    (flycheck-buttercup-should-syntax-check
     "language/processing/syntax_error/syntax_error.pde" 'processing-mode
     '(4 2 error "Syntax error, maybe a missing semicolon?"
         :checker processing))))

;;; test-processing.el ends here
