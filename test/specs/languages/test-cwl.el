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
           :checker cwl))))

  (describe "reading the tool's output"
    ;; Read from output recorded earlier, so this runs whether or
    ;; not the tool is installed here

    (flycheck-buttercup-def-parse-test cwl "language/cwl/cwl.cwl"
      '(1 1 error "Object 'flycheck_cwl.cwl' is not valid because")
      '(2 1 error "* invalid field 'class', expected one of: 'name', 'inVocab', 'fields',")
      '(3 1 error "* invalid field 'baseCommand', expected one of: 'name', 'inVocab',")
      '(4 1 error "* invalid field 'inputs', expected one of: 'name', 'inVocab', 'fields',")
      '(8 1 error "* invalid field 'outputs', expected one of: 'name', 'inVocab', 'fields',")
      '(1 1 error "- tried 'SaladEnumSchema' but")
      '(2 1 error "* invalid field 'class', expected one of: 'name', 'inVocab', 'symbols',")
      '(3 1 error "* invalid field 'baseCommand', expected one of: 'name', 'inVocab',")
      '(4 1 error "* invalid field 'inputs', expected one of: 'name', 'inVocab', 'symbols',")
      '(8 1 error "* invalid field 'outputs', expected one of: 'name', 'inVocab', 'symbols',")
      '(1 1 error "- tried 'SaladMapSchema' but")
      '(2 1 error "* invalid field 'class', expected one of: 'name', 'inVocab', 'type',")
      '(3 1 error "* invalid field 'baseCommand', expected one of: 'name', 'inVocab',")
      '(4 1 error "* invalid field 'inputs', expected one of: 'name', 'inVocab', 'type',")
      '(8 1 error "* invalid field 'outputs', expected one of: 'name', 'inVocab', 'type',")
      '(1 1 error "- tried 'SaladUnionSchema' but")
      '(2 1 error "* invalid field 'class', expected one of: 'name', 'inVocab', 'names',")
      '(3 1 error "* invalid field 'baseCommand', expected one of: 'name', 'inVocab',")
      '(4 1 error "* invalid field 'inputs', expected one of: 'name', 'inVocab', 'names',")
      '(8 1 error "* invalid field 'outputs', expected one of: 'name', 'inVocab', 'names',")
      '(1 1 error "- tried 'Documentation' but")
      '(2 1 error "* invalid field 'class', expected one of: 'name', 'inVocab', 'doc',")
      '(3 1 error "* invalid field 'baseCommand', expected one of: 'name', 'inVocab', 'doc',")
      '(4 1 error "* invalid field 'inputs', expected one of: 'name', 'inVocab', 'doc',")
      '(8 1 error "* invalid field 'outputs', expected one of: 'name', 'inVocab', 'doc',"))))

;;; test-cwl.el ends here
