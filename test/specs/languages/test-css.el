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
          (expect args :to-contain "json")))))

  (describe "Fatal-failure handling"
    ;; Stylelint inverts the usual convention: 2 is its findings status,
    ;; while 78 means it found no configuration, which is by far the most
    ;; common way it fails
    (it "disables the checker when stylelint has no configuration"
      (expect (flycheck--stylelint-handle-suspicious
               'css-stylelint 78 "No configuration provided for style.css")
              :to-equal
              '(disable . "No configuration provided for style.css")))

    (it "disables the checker on a bad invocation"
      (expect (car (flycheck--stylelint-handle-suspicious
                    'css-stylelint 64 "Unknown argument: --nope"))
              :to-be 'disable))

    (it "does not mistake stylelint's findings status for a failure"
      (expect (flycheck--stylelint-handle-suspicious 'css-stylelint 2 "")
              :to-be 'suspicious))

    (it "covers every stylelint checker"
      (dolist (checker '(css-stylelint scss-stylelint
                         sass-stylelint less-stylelint))
        (expect (flycheck-checker-get checker 'handle-suspicious)
                :to-be 'flycheck--stylelint-handle-suspicious))))
)

;;; test-css.el ends here
