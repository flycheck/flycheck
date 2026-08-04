;;; test-groovy.el --- Flycheck Specs: Groovy -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Groovy"
  (flycheck-buttercup-def-checker-test groovy groovy syntax-error
    (require 'cl)
    (flycheck-buttercup-should-syntax-check
     "language/groovy.groovy" 'groovy-mode
     '(2 14 error "unexpected token: {" :checker groovy)))

  (describe "reading the tool's output"
    ;; Read from output recorded earlier, so this runs whether or
    ;; not the tool is installed here

    (flycheck-buttercup-def-parse-test groovy "language/groovy.groovy"
      '(2 14 error "unexpected token: {"))))

;;; test-groovy.el ends here
