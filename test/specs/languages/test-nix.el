;;; test-nix.el --- Flycheck Specs: Nix -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Nix"
  (flycheck-buttercup-def-checker-test nix nix nil
    (flycheck-buttercup-should-syntax-check
     "language/nix/syntax-error.nix" 'nix-mode
     '(3 1 error "syntax error, unexpected IN, expecting ';'," :checker nix)))

  (describe "the statix checker command"
    (it "appends flycheck-statix-args before the source file"
      (flycheck-buttercup-with-temp-buffer
        (let ((flycheck-statix-args '("--config" "statix.toml")))
          (let ((args (flycheck-checker-substituted-arguments 'statix)))
            (expect args :to-contain "--config")
            (expect args :to-contain "statix.toml"))))))

)

;;; test-nix.el ends here
