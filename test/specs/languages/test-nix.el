;;; test-nix.el --- Flycheck Specs: Nix -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Nix"
  (flycheck-buttercup-def-checker-test nix nix nil
    (flycheck-buttercup-should-syntax-check
     "language/nix/syntax-error.nix" 'nix-mode
     '(3 1 error "syntax error, unexpected IN, expecting ';'," :checker nix)))

  (flycheck-buttercup-def-checker-test nix-linter nix nil
    (flycheck-buttercup-should-syntax-check
     "language/nix/warnings.nix" 'nix-mode
     '(1 1 warning "LetInInheritRecset" :id "LetInInheritRecset" :checker nix-linter)
     '(3 4 warning "Unneeded `rec` on set" :id "UnneededRec" :checker nix-linter))))

;;; test-nix.el ends here
