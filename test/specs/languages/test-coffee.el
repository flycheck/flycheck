;;; test-coffee.el --- Flycheck Specs: CoffeeScript -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language CoffeeScript"
  (flycheck-buttercup-def-checker-test coffee coffee syntax-error
    (flycheck-buttercup-should-syntax-check
     "language/coffee/syntax-error.coffee" 'coffee-mode
     '(4 7 error "missing \"" :checker coffee)))

)

;;; test-coffee.el ends here
