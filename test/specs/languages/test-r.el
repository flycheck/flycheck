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
  ;; Flycheck does not support Windows officially, and this is one of
  ;; the differences we do not chase (see the contributor guide)
  (assume (not (eq system-type 'windows-nt))
          "R reports this error at a different column on Windows")
    (let ((flycheck-disabled-checkers '(r-lintr)))
      (flycheck-buttercup-should-syntax-check
       "language/r.R" 'R-mode
       '(8 0 error "unexpected end of input" :checker r))))

  (describe "reading the tool's output"
    ;; Read from output recorded earlier, so this runs whether or
    ;; not the tool is installed here

    (flycheck-buttercup-def-parse-test r "language/r.R"
      '(8 nil error "unexpected end of input"))))

;;; test-r.el ends here
