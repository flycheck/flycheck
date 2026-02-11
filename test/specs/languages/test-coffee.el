;;; test-coffee.el --- Flycheck Specs: CoffeeScript -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language CoffeeScript"
  (flycheck-buttercup-def-checker-test coffee coffee syntax-error
    (flycheck-buttercup-should-syntax-check
     "language/coffee/syntax-error.coffee" 'coffee-mode
     '(4 7 error "missing \"" :checker coffee)))

  (flycheck-buttercup-def-checker-test coffee-coffeelint coffee error
    (flycheck-buttercup-should-syntax-check
     "language/coffee/error.coffee" 'coffee-mode
     '(4 nil error "Throwing strings is forbidden; context:"
         :checker coffee-coffeelint)))

  (flycheck-buttercup-def-checker-test coffee-coffeelint coffee warning
    (let ((flycheck-coffeelintrc "lint.json"))
      (flycheck-buttercup-should-syntax-check
       "language/coffee/error.coffee" 'coffee-mode
       '(4 nil warning "Throwing strings is forbidden; context:"
           :checker coffee-coffeelint)))))

;;; test-coffee.el ends here
