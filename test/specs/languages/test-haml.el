;;; test-haml.el --- Flycheck Specs: HAML -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language HAML"
  (flycheck-buttercup-def-checker-test haml haml "haml-error"
    (flycheck-buttercup-should-syntax-check
     "language/haml/haml-error.haml" 'haml-mode
     '(5 nil error "Inconsistent indentation: 3 spaces used for indentation, but the rest of the document was indented using 2 spaces."
         :checker haml)))

  (flycheck-buttercup-def-checker-test haml haml "ruby-error"
    (flycheck-buttercup-should-syntax-check
     "language/haml/ruby-error.haml" 'haml-mode
     '(1 nil error "unexpected end-of-input"
         :checker haml))))

;;; test-haml.el ends here
