;;; test-sass.el --- Flycheck Specs: Sass -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Sass"
  (flycheck-buttercup-def-checker-test sass sass syntax-error
    (flycheck-buttercup-should-syntax-check
     "language/sass/syntax-error.sass" 'sass-mode
     '(3 1 error "Inconsistent indentation, expected 4 spaces."
         :checker sass)))

  (flycheck-buttercup-def-checker-test sass sass warning
    (let ((flycheck-disabled-checkers '(sass-stylelint)))
      (flycheck-buttercup-should-syntax-check
       "language/sass/warning.sass" 'sass-mode
       '(2 3 warning "this is deprecated" :checker sass))))

  (flycheck-buttercup-def-checker-test sass-stylelint sass nil
    (let ((flycheck-disabled-checkers '(sass))
          (flycheck-stylelint-config
           (flycheck-buttercup-resource-filename
            "language/css/.stylelintrc.json")))
      (flycheck-buttercup-should-syntax-check
       "language/sass/error.sass" 'sass-mode
       '(1 1 error "Unknown word .a (CssSyntaxError)"
           :id "CssSyntaxError" :checker sass-stylelint))))

  (describe "Fatal-failure handling"
    ;; Exit 64 is Dart Sass refusing the invocation, which is what a
    ;; release too old to know --silence-deprecation, or the long-dead
    ;; Ruby Sass answering to the same executable name, says about the
    ;; flags these checkers pass
    (it "disables the checker on flags the tool does not know"
      (expect (flycheck--sass-handle-suspicious
               'sass 64
               "Could not find an option named \"--silence-deprecation\".")
              :to-equal
              '(disable . "Could not find an option named \
\"--silence-deprecation\".")))

    (it "does not mistake a stylesheet that does not compile for a failure"
      (expect (flycheck--sass-handle-suspicious 'sass 65 "Error: expected \"{\".")
              :to-be 'suspicious))

    (it "covers both Dart Sass checkers"
      (dolist (checker '(sass scss))
        (expect (flycheck-checker-get checker 'handle-suspicious)
                :to-be 'flycheck--sass-handle-suspicious))))

  (describe "reading the tool's output"
    ;; Read from output recorded earlier, so this runs whether or
    ;; not the tool is installed here

    (flycheck-buttercup-def-parse-test sass "language/sass/syntax-error.sass"
      '(3 1 error "Inconsistent indentation, expected 4 spaces."))

    (flycheck-buttercup-def-parse-test sass "language/sass/warning.sass"
      '(2 3 warning "this is deprecated"))

    (flycheck-buttercup-def-parse-test sass-stylelint "language/sass/error.sass"
      '(1 1 error "Unknown word // (CssSyntaxError)" :id "CssSyntaxError"))))

;;; test-sass.el ends here
