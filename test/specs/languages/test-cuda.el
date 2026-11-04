;;; test-cuda.el --- Flycheck Specs: CUDA -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language CUDA"
  (flycheck-buttercup-def-checker-test cuda-nvcc cuda syntax-error
    (flycheck-buttercup-should-syntax-check
     "language/cuda/syntax-error.cu" 'cuda-mode
     '(5 0 error "error: identifier \"ac\" is undefined"
         :checker cuda)))

  (flycheck-buttercup-def-checker-test cuda-nvcc cuda syntax-warning
    (flycheck-buttercup-should-syntax-check
     "language/cuda/syntax-warning.cu" 'cuda-mode
     '(3 0 warning "variable \"b\" was set but never used"
         :checker cuda)))

  (describe "the cuda-nvcc command"
    (it "writes its object file to the platform's null device"
      (flycheck-buttercup-with-temp-buffer
        (let* ((null-device "NUL")
               (args (flycheck-checker-substituted-arguments 'cuda-nvcc)))
          (expect (cadr (member "--output-file" args))
                  :to-equal "NUL"))))))

;;; test-cuda.el ends here
