;;; test-r.el --- Flycheck Specs: R -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language R"
  (flycheck-buttercup-def-checker-test r-lintr r nil
    (assume (flycheck-r-has-lintr (flycheck-checker-executable 'r-lintr)))
    (let ((flycheck-lintr-caching nil))
      (flycheck-buttercup-should-syntax-check
       "language/r.R" 'R-mode
       '(1 28 info "Opening curly braces should never go on their own line and should always be followed by a new line."
           :checker r-lintr)
       '(1 56 info "Put spaces around all infix operators." :checker r-lintr)
       '(4 6 warning "Do not use absolute paths." :checker r-lintr)
       '(7 5 error "unexpected end of input" :checker r-lintr))))

  (flycheck-buttercup-def-checker-test r r nil
    (let ((flycheck-disabled-checkers '(r-lintr)))
      (flycheck-buttercup-should-syntax-check
       "language/r.R" 'R-mode
       '(8 0 error "unexpected end of input" :checker r)))))

;;; test-r.el ends here
