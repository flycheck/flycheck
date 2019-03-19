;;; test-go.el --- Flycheck Specs: Go -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; This file implements buttercup tests for the `go-staticheck' syntax checker.
;;
;;; Code:

(require 'flycheck-buttercup)

(describe "Language Go"
  (describe "The staticcheck error parser"
    (let ((json "
{
  \"code\":\"compile\",
  \"severity\":\"error\",
  \"location\": {
    \"file\":\"/home/gastove/golang/src/github.com/Gastove/test/pkg/lib/lib.go\",
    \"line\":4,
    \"column\":8
  },
  \"message\":\"expected ';', found ':'\"}
{
  \"code\":\"compile\",
  \"severity\":\"warning\",
  \"location\": {
    \"file\":\"/home/gastove/golang/src/github.com/Gastove/test/pkg/lib/lib.go\",
    \"line\":4,
    \"column\":2
  },
  \"message\":\"undeclared name: Number\"
}"
                ))

      (it "parses staticcheck JSON output"
        (expect (flycheck-parse-go-staticcheck json 'checker 'buffer)
                :to-be-equal-flycheck-errors
                (list
                 (flycheck-error-new-at
                  4 8 'error
                  "expected ';', found ':'"
                  :id "compile"
                  :checker 'checker
                  :buffer 'buffer
                  :filename "/home/gastove/golang/src/github.com/Gastove/test/pkg/lib/lib.go")
                 (flycheck-error-new-at
                  4 2 'warning
                  "undeclared name: Number"
                  :id "compile"
                  :checker 'checker
                  :buffer 'buffer
                  :filename "/home/gastove/golang/src/github.com/Gastove/test/pkg/lib/lib.go")
                 ))))))

;;; test-go.el ends here
