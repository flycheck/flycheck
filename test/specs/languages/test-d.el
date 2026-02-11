;;; test-d.el --- Flycheck Specs: D -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language D"
  (flycheck-buttercup-def-checker-test d-dmd d warning-include-path
    (let ((flycheck-dmd-include-path '("../../lib")))
      (flycheck-buttercup-should-syntax-check
       "language/d/src/dmd/warning.d" 'd-mode
       '(9 5 warning "statement is not reachable" :checker d-dmd)
       '(20 17 warning "function `dmd.warning.bar` is deprecated"
            :checker d-dmd))))

  (flycheck-buttercup-def-checker-test d-dmd d missing-import
    (flycheck-buttercup-should-syntax-check
     "language/d/src/dmd/warning.d" 'd-mode
     '(4 8 error "module `external_library` is in file 'external_library.d' which cannot be read"
         :checker d-dmd)))

  (flycheck-buttercup-def-checker-test d-dmd d continuation-line
    (flycheck-buttercup-should-syntax-check
     "language/d/src/dmd/continuation.d" 'd-mode
     '(5 12 error "undefined identifier `invalid`"
         :checker d-dmd)
     '(10 12 error "template instance `continuation.T!()` error instantiating"
          :checker d-dmd)
     '(13 1 info "instantiated from here: `U!()`"
          :checker d-dmd)))

  (flycheck-buttercup-def-checker-test d-dmd d non-d-extension
    (assume (fboundp 'd-mode))
    (flycheck-buttercup-with-temp-buffer
      (insert "!invalid")
      (d-mode)
      (flycheck-buttercup-buffer-sync)
      (flycheck-buttercup-should-errors
       '(1 1 error "declaration expected, not `!`"
           :checker d-dmd)))))

;;; test-d.el ends here
