;;; test-css.el --- Flycheck Specs: CSS -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language CSS"
  (flycheck-buttercup-def-checker-test css-stylelint css syntax-error
    (let ((flycheck-stylelintrc
           (flycheck-buttercup-resource-filename
            "language/css/.stylelintrc.json")))
      (flycheck-buttercup-should-syntax-check
       "language/css/syntax-error.css" 'css-mode
       '(4 5 error "Unknown word font-size (CssSyntaxError)"
           :id "CssSyntaxError" :checker css-stylelint)))))

;;; test-css.el ends here
