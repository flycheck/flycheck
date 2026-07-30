;;; test-css.el --- Flycheck Specs: CSS -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language CSS"
  (flycheck-buttercup-def-checker-test css-stylelint css syntax-error
    (let ((flycheck-stylelint-config
           (flycheck-buttercup-resource-filename
            "language/css/.stylelintrc.json")))
      (flycheck-buttercup-should-syntax-check
       "language/css/syntax-error.css" 'css-mode
       '(4 5 error "Unknown word font-size (CssSyntaxError)"
           :id "CssSyntaxError" :checker css-stylelint))))

  (describe "the stylelint checker command"
    (it "always requests JSON output"
      (let ((flycheck-stylelint-config nil)
            (flycheck-stylelint-args nil))
        (expect (flycheck-checker-substituted-arguments 'css-stylelint)
                :to-contain "--formatter")
        (expect (flycheck-checker-substituted-arguments 'css-stylelint)
                :to-contain "json")))

    (it "appends flycheck-stylelint-args after the JSON formatter"
      (let ((flycheck-stylelint-config nil)
            (flycheck-stylelint-args '("--custom-syntax" "postcss-scss")))
        (let ((args (flycheck-checker-substituted-arguments 'scss-stylelint)))
          (expect args :to-contain "--custom-syntax")
          (expect args :to-contain "postcss-scss")
          ;; the mandatory JSON formatter is still there
          (expect args :to-contain "json"))))))

;;; test-css.el ends here
