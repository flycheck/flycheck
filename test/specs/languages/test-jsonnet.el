;;; test-jsonnet.el --- Flycheck Specs: Jsonnet -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(defun test-jsonnet/go-implementation-p ()
  "Whether the jsonnet the checker would run is go-jsonnet."
  (when-let* ((jsonnet (flycheck-find-checker-executable 'jsonnet)))
    (with-temp-buffer
      (call-process jsonnet nil t nil "--version")
      (goto-char (point-min))
      (search-forward "Go implementation" nil 'noerror))))

(describe "Language Jsonnet"
  ;; The two jsonnet implementations word their static errors
  ;; differently, so each gets its own live check
  (flycheck-buttercup-def-checker-test jsonnet jsonnet static
    (assume (test-jsonnet/go-implementation-p)
            "The installed jsonnet is not go-jsonnet")
    (flycheck-buttercup-should-syntax-check
     "language/jsonnet/static_error.jsonnet" 'jsonnet-mode
     '(1 8 error "Block string literals not allowed in imports"
         :end-line 3 :end-column 4 :checker jsonnet)))

  (flycheck-buttercup-def-checker-test jsonnet jsonnet static-cpp
    (assume (not (test-jsonnet/go-implementation-p))
            "The installed jsonnet is not the C++ implementation")
    (flycheck-buttercup-should-syntax-check
     "language/jsonnet/static_error.jsonnet" 'jsonnet-mode
     '(1 8 error "Cannot use text blocks in import statements."
         :end-line 3 :end-column 4 :checker jsonnet)))

  (flycheck-buttercup-def-checker-test jsonnet jsonnet runtime
    (flycheck-buttercup-should-syntax-check
     "language/jsonnet/runtime_error.jsonnet" 'jsonnet-mode
     '(2 6 error "Field does not exist: flat" :checker jsonnet
         :end-line 2 :end-column 14)))

  (describe "the jsonnet checker command"
    (it "passes the external code files before the extra arguments"
      (flycheck-buttercup-with-temp-buffer
        (let ((flycheck-jsonnet-include-paths nil)
              (flycheck-jsonnet-ext-code-files '("config=cfg.jsonnet"))
              (flycheck-jsonnet-args nil))
          (let ((args (flycheck-checker-substituted-arguments 'jsonnet)))
            (expect args :to-contain "--ext-code-file")
            (expect args :to-contain "config=cfg.jsonnet"))))))

  (describe "reading the tool's output"
    ;; go-jsonnet has never printed the STATIC ERROR prefix the C++
    ;; binary uses, so each format gets its own spec

    (flycheck-buttercup-def-parse-test jsonnet "language/jsonnet/static_error.jsonnet"
      '(1 8 error "Block string literals not allowed in imports"
          :end-line 3 :end-column 4))

    (it "reads go-jsonnet's bare static errors, span form"
      (flycheck-buttercup-with-temp-buffer
        (expect
         (flycheck-buttercup-parse
          'jsonnet
          "static_error.jsonnet:(1:8)-(3:4) Block string literals not allowed in imports

import |||
  xyz
|||
")
         :to-be-equal-flycheck-errors
         (list (flycheck-error-new-at
                1 8 'error "Block string literals not allowed in imports"
                :end-line 3 :end-column 4
                :checker 'jsonnet :buffer (current-buffer)
                :filename "static_error.jsonnet")))))

    (it "reads go-jsonnet's bare static errors, single-position form"
      (flycheck-buttercup-with-temp-buffer
        (expect
         (flycheck-buttercup-parse
          'jsonnet
          "old_static.jsonnet:1:23-24 Not a unary operator: =

local x = 1; x == = 2
")
         :to-be-equal-flycheck-errors
         (list (flycheck-error-new-at
                1 23 'error "Not a unary operator: ="
                :end-line 1 :end-column 24
                :checker 'jsonnet :buffer (current-buffer)
                :filename "old_static.jsonnet")))))

    (it "still reads the C++ binary's prefixed static errors"
      (flycheck-buttercup-with-temp-buffer
        (expect
         (flycheck-buttercup-parse
          'jsonnet
          "STATIC ERROR: static_error.jsonnet:(1:8)-(3:4): Cannot use text blocks in import statements.
")
         :to-be-equal-flycheck-errors
         (list (flycheck-error-new-at
                1 8 'error "Cannot use text blocks in import statements."
                :end-line 3 :end-column 4
                :checker 'jsonnet :buffer (current-buffer)
                :filename "static_error.jsonnet")))))))

;;; test-jsonnet.el ends here
