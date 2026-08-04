;;; test-ada.el --- Flycheck Specs: Ada -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Ada"
  (flycheck-buttercup-def-checker-test ada-gnat ada syntax-error
    (require 'speedbar)
    (flycheck-buttercup-should-syntax-check
     "language/ada/syntaxerror.adb" 'ada-mode
     '(7 32 error "missing \";\"" :checker ada-gnat)
     '(8 5 error "misspelling of \"SYNTAXERROR\"" :checker ada-gnat)))

  (flycheck-buttercup-def-checker-test ada-gnat ada warnings
    (require 'speedbar)
    (flycheck-buttercup-should-syntax-check
     "language/ada/hello.adb" 'ada-mode
     '(6 4 warning "variable \"Name\" is not referenced" :checker ada-gnat)
     '(8 11 warning "unrecognized pragma \"Foo\"" :checker ada-gnat)))

  (describe "reading the tool's output"
    ;; Read from output recorded earlier, so this runs whether or
    ;; not the tool is installed here

    (flycheck-buttercup-def-parse-test ada-gnat "language/ada/syntaxerror.adb"
      '(7 32 error "error: missing \";\"")
      '(8 5 error "error: misspelling of \"SYNTAXERROR\""))))

;;; test-ada.el ends here
