;;; test-cuda.el --- Flycheck Specs: CUDA -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language CUDA"
  (flycheck-buttercup-def-checker-test cuda cuda syntax-error
    (flycheck-buttercup-should-syntax-check
     "language/cuda/syntax-error.cu" 'cuda-mode
     '(5 0 error "error: identifier \"ac\" is undefined"
         :checker cuda)))

  (flycheck-buttercup-def-checker-test cuda cuda syntax-warning
    (flycheck-buttercup-should-syntax-check
     "language/cuda/syntax-warning.cu" 'cuda-mode
     '(3 0 warning "variable \"b\" was set but never used"
         :checker cuda))))

;;; test-cuda.el ends here
