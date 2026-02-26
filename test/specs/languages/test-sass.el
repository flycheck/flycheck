;;; test-sass.el --- Flycheck Specs: Sass -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Sass"
  (flycheck-buttercup-def-checker-test sass-stylelint sass nil
    (let ((flycheck-stylelintrc
           (flycheck-buttercup-resource-filename
            "language/css/.stylelintrc.json")))
      (flycheck-buttercup-should-syntax-check
       "language/sass/error.sass" 'sass-mode
       '(1 1 error "Unknown word .a (CssSyntaxError)"
           :id "CssSyntaxError" :checker sass-stylelint)))))

;;; test-sass.el ends here
