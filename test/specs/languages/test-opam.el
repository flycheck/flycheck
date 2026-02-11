;;; test-opam.el --- Flycheck Specs: OPAM -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language OPAM"
  (flycheck-buttercup-def-checker-test opam opam nil
    (flycheck-buttercup-should-syntax-check
     "language/opam.opam" 'tuareg-opam-mode
     '(0 nil error "Missing field 'maintainer'"
         :id "23" :checker opam)
     '(0 nil warning "Missing field 'authors'"
         :id "25" :checker opam)
     '(0 nil warning "Missing field 'homepage'"
         :id "35" :checker opam)
     '(0 nil warning "Missing field 'bug-reports'"
         :id "36" :checker opam)
     '(2 1 error "Invalid field maintainers"
         :id "3" :checker opam))))

;;; test-opam.el ends here
