;;; test-luau.el --- Flycheck Specs: Luau -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Luau"
  (flycheck-buttercup-def-checker-test luau-analyze luau syntax-error
    (flycheck-buttercup-should-syntax-check
     "language/luau/syntax-error.luau" 'lua-mode
     '(5 1 warning "SyntaxError: Incomplete statement: expected assignment or a function call" :id "W0" :checker luau-analyze)
     (5 7 warning "SameLineStatement: A new statement is on the same line; add semi-colon on previous statement to silence" :id "W0" :checker luau-analyze)))

  (flycheck-buttercup-def-checker-test luau-analyze luau warnings
    (flycheck-buttercup-should-syntax-check
     "language/luau/warnings.luau" 'lua-mode
     '(3 16 warning "FunctionUnused: Function 'test' is never used; prefix with '_' to silence"
         :id "W0" :checker luau-analyze)
     '(5 12 warning "UninitializedLocal: Variable 'var2' defined at line 4 is never initialized or assigned; initialize with 'nil' to silence"
         :id "W0" :checker luau-analyze))))

;;; test-luau.el ends here
