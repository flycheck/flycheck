;;; test-scss.el --- Flycheck Specs: SCSS -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language SCSS"
  (flycheck-buttercup-def-checker-test scss-stylelint scss syntax-error
    (let ((flycheck-stylelint-config
           (flycheck-buttercup-resource-filename
            "language/css/.stylelintrc.json")))
      (flycheck-buttercup-should-syntax-check
       "language/scss/error.scss" 'scss-mode
       '(3 11 error "Unknown word olor (CssSyntaxError)"
           :id "CssSyntaxError" :checker scss-stylelint))))

  (describe "reading the tool's output"
    ;; Read from output recorded earlier, so this runs whether or
    ;; not the tool is installed here

    (flycheck-buttercup-def-parse-test scss-stylelint "language/scss/error.scss"
      '(1 nil error "Unexpected token 'with'" :id "SyntaxError"))))

;;; test-scss.el ends here
