;;; test-ocaml.el --- Flycheck Specs: OCaml -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language OCaml"
  (flycheck-buttercup-def-checker-test ocaml ocaml syntax-error
    (flycheck-buttercup-should-syntax-check
     "language/ocaml/syntaxerror.ml" 'tuareg-mode
     '(4 0 error "Syntax error" :checker ocaml)))

  (flycheck-buttercup-def-checker-test ocaml ocaml warning
    (flycheck-buttercup-should-syntax-check
     "language/ocaml/warning.ml" 'tuareg-mode
     '(3 8 warning "this optional argument cannot be erased."
         :id "16" :checker ocaml))))

;;; test-ocaml.el ends here
