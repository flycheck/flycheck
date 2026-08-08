;;; test-scss.el --- Flycheck Specs: SCSS -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language SCSS"
  (flycheck-buttercup-def-checker-test scss scss syntax-error
    (flycheck-buttercup-should-syntax-check
     "language/scss/error.scss" 'scss-mode
     '(3 20 error "expected \"{\"." :checker scss)))

  (flycheck-buttercup-def-checker-test scss scss warnings
    (let ((flycheck-disabled-checkers '(scss-stylelint)))
      (flycheck-buttercup-should-syntax-check
       "language/scss/warning.scss" 'scss-mode
       '(2 3 warning ".container is deprecated" :checker scss)
       '(3 15 warning "Using / for division outside of calc() is deprecated \
and will be removed in Dart Sass 2.0.0."
           :id "slash-div" :checker scss))))

  (flycheck-buttercup-def-checker-test scss-stylelint scss syntax-error
    (let ((flycheck-disabled-checkers '(scss))
          (flycheck-stylelint-config
           (flycheck-buttercup-resource-filename
            "language/css/.stylelintrc.json")))
      (flycheck-buttercup-should-syntax-check
       "language/scss/error.scss" 'scss-mode
       '(3 11 error "Unknown word olor (CssSyntaxError)"
           :id "CssSyntaxError" :checker scss-stylelint))))

  (describe "reading the tool's output"
    ;; Read from output recorded earlier, so this runs whether or
    ;; not the tool is installed here

    (flycheck-buttercup-def-parse-test scss "language/scss/error.scss"
      '(3 20 error "expected \"{\"."))

    (flycheck-buttercup-def-parse-test scss "language/scss/warning.scss"
      '(2 3 warning ".container is deprecated")
      '(3 15 warning "Using / for division outside of calc() is deprecated \
and will be removed in Dart Sass 2.0.0."
          :id "slash-div"))

    (flycheck-buttercup-def-parse-test scss-stylelint "language/scss/error.scss"
      '(3 11 error "Unknown word olor (CssSyntaxError)" :id "CssSyntaxError"))))

;;; test-scss.el ends here
