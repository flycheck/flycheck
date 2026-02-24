;;; test-scss.el --- Flycheck Specs: SCSS -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language SCSS"
  (flycheck-buttercup-def-checker-test scss-stylelint scss syntax-error
    (let ((flycheck-stylelintrc
           (flycheck-buttercup-resource-filename
            "language/css/.stylelintrc.json")))
      (flycheck-buttercup-should-syntax-check
       "language/scss/error.scss" 'scss-mode
       '(3 11 error "Unknown word olor (CssSyntaxError)"
           :id "CssSyntaxError" :checker scss-stylelint)))))

;;; test-scss.el ends here
