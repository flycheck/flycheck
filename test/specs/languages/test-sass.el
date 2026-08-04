;;; test-sass.el --- Flycheck Specs: Sass -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Sass"
  (flycheck-buttercup-def-checker-test sass-stylelint sass nil
    (let ((flycheck-stylelint-config
           (flycheck-buttercup-resource-filename
            "language/css/.stylelintrc.json")))
      (flycheck-buttercup-should-syntax-check
       "language/sass/error.sass" 'sass-mode
       '(1 1 error "Unknown word .a (CssSyntaxError)"
           :id "CssSyntaxError" :checker sass-stylelint))))

  (describe "reading the tool's output"
    ;; Read from output recorded earlier, so this runs whether or
    ;; not the tool is installed here

    (flycheck-buttercup-def-parse-test sass-stylelint "language/sass/error.sass"
      '(1 1 error "Unknown word // (CssSyntaxError)" :id "CssSyntaxError"))))

;;; test-sass.el ends here
