;;; test-cwl.el --- Flycheck Specs: CWL -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language CWL"
  (flycheck-buttercup-def-checker-test cwl cwl syntax-error
    (let ((flycheck-cwl-schema-path "schema/CommonWorkflowLanguage.yml"))
      (flycheck-buttercup-should-syntax-check
       "language/cwl/cwl.cwl" 'cwl-mode
       '(6 5 error "value is a str, expected null or CommandLineBinding"
           :checker cwl)))))

;;; test-cwl.el ends here
