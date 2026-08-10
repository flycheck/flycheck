;;; test-r.el --- Flycheck Specs: R -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language R"
  (flycheck-buttercup-def-checker-test r-lintr r nil
    (assume (flycheck-r-has-lintr (flycheck-checker-executable 'r-lintr)))
    (let ((flycheck-lintr-caching nil))
      ;; A parse error makes lintr stop linting, so style coverage
      ;; needs a file that parses; r.R keeps covering the error path
      ;; through the recorded output below
      (flycheck-buttercup-should-syntax-check
       "language/r-style.R" 'R-mode
       '(1 3 info "Use one of <-, <<- for assignment, not =."
           :id "assignment_linter" :checker r-lintr)
       '(2 3 info "Place a space before left parenthesis, except in a function call."
           :id "spaces_left_parentheses_linter" :checker r-lintr))))

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
      '(8 nil error "unexpected end of input"))
    (flycheck-buttercup-def-parse-test r-lintr "language/r-style.R"
      '(1 3 info "Use one of <-, <<- for assignment, not =."
          :id "assignment_linter")
      '(2 3 info "Place a space before left parenthesis, except in a function call."
          :id "spaces_left_parentheses_linter"))
    (flycheck-buttercup-def-parse-test r-lintr "language/r.R"
      '(7 6 error "unexpected end of input"))))

;;; test-r.el ends here
