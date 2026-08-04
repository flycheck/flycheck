;;; test-llvm.el --- Flycheck Specs: LLVM -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language LLVM"
  (flycheck-buttercup-def-checker-test llvm-llc llvm nil
    (flycheck-buttercup-should-syntax-check
     "language/llvm.ll" 'llvm-mode
     '(4 19 error "'%tmp' defined with type 'i32'" :checker llvm-llc)))

  (describe "reading the tool's output"
    ;; Read from output recorded earlier, so this runs whether or
    ;; not the tool is installed here

    (flycheck-buttercup-def-parse-test llvm-llc "language/llvm.ll"
      '(4 19 error "'%tmp' defined with type 'i32' but expected 'i64'"))))

;;; test-llvm.el ends here
