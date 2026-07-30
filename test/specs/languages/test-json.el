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

  (describe "the json-jq checker command"
    (it "threads flycheck-json-jq-args in before the filter"
      (let ((flycheck-json-jq-args '("--seq")))
        (expect (flycheck-checker-substituted-arguments 'json-jq)
                :to-contain "--seq")))))

;;; test-json.el ends here
