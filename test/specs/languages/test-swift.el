;;; test-swift.el --- Flycheck Specs: Swift -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Swift"
  (flycheck-buttercup-def-checker-test swift swift syntax-error
    (flycheck-buttercup-should-syntax-check
     "language/swift/syntaxerror.swift" 'swift-mode
     '(5 1 error "expected ')' in expression list" :checker swift)
     '(4 8 info "to match this opening '('" :checker swift)
     '(5 1 error "expected '}' at end of brace statement" :checker swift)
     '(3 14 info "to match this opening '{'" :checker swift)))

  (describe "reading the tool's output"
    ;; Read from output recorded earlier, so this runs whether or
    ;; not the tool is installed here

    (flycheck-buttercup-def-parse-test swift "language/swift/syntaxerror.swift"
      '(5 1 error "expected ')' in expression list")
      '(4 8 info "to match this opening '('")
      '(5 1 error "expected '}' at end of brace statement")
      '(3 14 info "to match this opening '{'"))))

;;; test-swift.el ends here
