;;; test-llvm.el --- Flycheck Specs: LLVM -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language LLVM"
  (flycheck-buttercup-def-checker-test llvm-llc llvm nil
    (flycheck-buttercup-should-syntax-check
     "language/llvm.ll" 'llvm-mode
     '(4 19 error "'%tmp' defined with type 'i32'" :checker llvm-llc))))

;;; test-llvm.el ends here
