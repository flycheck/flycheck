;;; test-jsonnet.el --- Flycheck Specs: Jsonnet -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Jsonnet"
  (flycheck-buttercup-def-checker-test jsonnet jsonnet static
    (flycheck-buttercup-should-syntax-check
     "language/jsonnet/static_error.jsonnet" 'jsonnet-mode
     '(1 23 "Not a unary operator: =" :checker jsonnet)))

  (flycheck-buttercup-def-checker-test jsonnet jsonnet runtime
    (flycheck-buttercup-should-syntax-check
     "language/jsonnet/runtime_error.jsonnet" 'jsonnet-mode
     '(2 6 "Field does not exist: flat" :checker jsonnet
         :end-line 2 :end-column 14))))

;;; test-jsonnet.el ends here
