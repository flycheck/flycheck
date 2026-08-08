;;; test-luau.el --- Flycheck Specs: Luau -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Luau"
  (flycheck-buttercup-def-checker-test luau-analyze luau syntax-error
    (flycheck-buttercup-should-syntax-check
     "language/luau/syntax-error.luau" 'lua-mode
     '(7 1 error "Expected 'end' (to close 'then' at line 5), got <eof>"
         :id "SyntaxError" :checker luau-analyze)))

  (flycheck-buttercup-def-checker-test luau-analyze luau type-error
    (flycheck-buttercup-should-syntax-check
     "language/luau/type-error.luau" 'lua-mode
     '(6 19 error "Expected this to be 'number', but got 'string'"
         :id "TypeError" :checker luau-analyze)))

  (flycheck-buttercup-def-checker-test luau-analyze luau warnings
    (flycheck-buttercup-should-syntax-check
     "language/luau/warnings.luau" 'lua-mode
     '(5 16 warning "Function 'unused' is never used; prefix with '_' to silence"
         :id "FunctionUnused" :checker luau-analyze)
     '(9 7 warning "Variable 'value' is never used; prefix with '_' to silence"
         :id "LocalUnused" :checker luau-analyze)))

  (describe "checker selection"

    (it "runs in a buffer visiting a .luau file"
      (flycheck-buttercup-with-resource-buffer "language/luau/warnings.luau"
        (expect (funcall (flycheck-checker-get 'luau-analyze 'predicate))
                :to-be-truthy)))

    (it "leaves a plain Lua buffer to the Lua checkers"
      ;; The analyzer rejects valid Lua, such as goto and labels, so it
      ;; must not hijack buffers that merely share the major mode
      (flycheck-buttercup-with-resource-buffer "language/lua/syntax-error.lua"
        (expect (funcall (flycheck-checker-get 'luau-analyze 'predicate))
                :not :to-be-truthy))))

  (describe "the luau-analyze checker command"
    (it "appends flycheck-luau-analyze-args before the source file"
      (flycheck-buttercup-with-temp-buffer
        (setq buffer-file-name (make-temp-file "flycheck-luau" nil ".luau"))
        (let ((flycheck-luau-analyze-args '("--mode=strict")))
          (expect (flycheck-checker-substituted-arguments 'luau-analyze)
                  :to-contain "--mode=strict")))))

  (describe "reading the tool's output"
    ;; Read from output recorded earlier, so this runs whether or
    ;; not the tool is installed here

    (flycheck-buttercup-def-parse-test luau-analyze "language/luau/syntax-error.luau"
      '(7 1 error "Expected 'end' (to close 'then' at line 5), got <eof>"
          :id "SyntaxError"))

    (flycheck-buttercup-def-parse-test luau-analyze "language/luau/type-error.luau"
      '(6 19 error "Expected this to be 'number', but got 'string'"
          :id "TypeError"))

    (flycheck-buttercup-def-parse-test luau-analyze "language/luau/warnings.luau"
      '(5 16 warning "Function 'unused' is never used; prefix with '_' to silence"
          :id "FunctionUnused")
      '(9 7 warning "Variable 'value' is never used; prefix with '_' to silence"
          :id "LocalUnused"))))

;;; test-luau.el ends here
