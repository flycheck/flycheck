;;; test-ember-template.el --- Flycheck Specs: Ember Template -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Ember Template"
  (flycheck-buttercup-def-checker-test ember-template ember-template error
    (flycheck-buttercup-should-syntax-check
     "language/ember-template-lint/ember-template-lint/error.hbs" 'web-mode
     '(2 16 error "Incorrect indentation for `<span>` beginning at L2:C16. Expected `<span>` to be at an indentation of 2 but was found at 16." :id "block-indentation" :checker ember-template)))

  (flycheck-buttercup-def-checker-test ember-template ember-template warning
    (flycheck-buttercup-should-syntax-check
     "language/ember-template-lint/ember-template-lint/warning.hbs" 'web-mode
     '(1 nil warning "Non-translated string used" :id "no-bare-strings" :checker ember-template))))

;;; test-ember-template.el ends here
