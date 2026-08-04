;;; test-coffee.el --- Flycheck Specs: CoffeeScript -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language CoffeeScript"
  (flycheck-buttercup-def-checker-test coffee coffee syntax-error
    (flycheck-buttercup-should-syntax-check
     "language/coffee/syntax-error.coffee" 'coffee-mode
     '(4 7 error "missing \"" :checker coffee)))



  (describe "reading the tool's output"
    ;; Read from output recorded earlier, so this runs whether or
    ;; not the tool is installed here

    (flycheck-buttercup-def-parse-test coffee "language/coffee/syntax-error.coffee"
      '(4 7 error "missing \""))))

;;; test-coffee.el ends here
