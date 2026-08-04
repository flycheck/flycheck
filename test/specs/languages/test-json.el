;;; test-json.el --- Flycheck Specs: JSON -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language JSON"
  (flycheck-buttercup-def-checker-test json-python-json json nil
    (flycheck-buttercup-should-syntax-check
     "language/json.json" 'json-mode
     '(1 44 error "Extra data" :checker json-python-json)))

  (flycheck-buttercup-def-checker-test json-jq json nil
    (let ((flycheck-disabled-checkers '(json-python-json)))
      (flycheck-buttercup-should-syntax-check
       "language/json.json" 'json-mode
       '(1 44 error "Expected value before ','" :checker json-jq))))

  (describe "reading jq's output"
    ;; Runs whether or not jq is installed, so a change to the patterns
    ;; cannot pass unnoticed on a machine that happens to lack it
    (flycheck-buttercup-def-parse-test json-jq "language/json.json"
      '(1 44 error "Expected value before ','"))

    (it "drops the program name jq 1.7 prefixes"
      (expect (flycheck-buttercup-fixture 'json-jq "language/json.json")
              :to-match "\\`jq: parse error: ")))

  (describe "the json-jq checker command"
    (it "threads flycheck-json-jq-args in before the filter"
      (let ((flycheck-json-jq-args '("--seq")))
        (expect (flycheck-checker-substituted-arguments 'json-jq)
                :to-contain "--seq"))))

  (describe "reading the tool's output"
    ;; Read from output recorded earlier, so this runs whether or
    ;; not the tool is installed here

    (flycheck-buttercup-def-parse-test json-python-json "language/json.json"
      '(1 44 error "Extra data"))))

;;; test-json.el ends here
