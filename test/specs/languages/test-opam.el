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
         :id "3" :checker opam)))

  (describe "reading the tool's output"
    ;; Read from output recorded earlier, so this runs whether or
    ;; not the tool is installed here

    (flycheck-buttercup-def-parse-test opam "language/opam.opam"
      '(2 1 error "Invalid field maintainers" :id "3")
      '(0 nil error "Missing field 'maintainer'" :id "23")
      '(0 nil warning "Missing field 'authors'" :id "25")
      '(0 nil warning "Missing field 'homepage'" :id "35")
      '(0 nil warning "Missing field 'bug-reports'" :id "36")
      '(0 nil error "Synopsis and description must not be both empty" :id "57"))))

;;; test-opam.el ends here
