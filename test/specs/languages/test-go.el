;;; test-go.el --- Flycheck Specs: Go -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; This file implements buttercup tests for the `go-staticheck' syntax checker.
;;
;;; Code:

(require 'flycheck-buttercup)
(require 'test-helpers)

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
                 )))))

  (describe "Checker tests"
    (flycheck-buttercup-def-checker-test go-gofmt go syntax-error
      (flycheck-buttercup-should-syntax-check
       "language/go/src/syntax/syntax-error.go" 'go-mode
       '(5 9 error "expected '(', found ta" :checker go-gofmt)
       '(6 1 error "expected declaration, found '}'" :checker go-gofmt)))

    (flycheck-buttercup-def-checker-test (go-build go-vet) go complete-chain
      (flycheck-buttercup-with-env
          `(("GOPATH" . ,(flycheck-buttercup-resource-filename "language/go")))
        (flycheck-buttercup-should-syntax-check
         "language/go/src/warnings.go" 'go-mode
         '(4 2 error "imported and not used: \"fmt\"" :checker go-build)
         '(8 2 error "undefined: fmt" :checker go-build)
         '(12 2 error "undefined: fmt" :checker go-build)
         '(17 2 error "undefined: fmt" :checker go-build)
         '(19 13 error "cannot use 1 (type untyped int) as type string in argument to Warnf"
              :checker go-build))))

    (flycheck-buttercup-def-checker-test go-build go handles-packages
      (flycheck-buttercup-with-env
          `(("GOPATH" . ,(flycheck-buttercup-resource-filename "language/go")))
        (flycheck-buttercup-should-syntax-check "language/go/src/b1/main.go" 'go-mode)))

    (flycheck-buttercup-def-checker-test go-build go missing-package
      (let ((go-root (or (getenv "GOROOT") "/usr/local/go"))
            (go-path (concat (getenv "HOME") "/go")))
        (flycheck-buttercup-with-env '(("GOPATH" . nil))
          (flycheck-buttercup-should-syntax-check
           "language/go/src/b1/main.go" 'go-mode
           `(4 2 error ,(format "cannot find package \"b2\" in any of:\n\t%s/src/b2 (from $GOROOT)\n\t%s/src/b2 (from $GOPATH)"
                                go-root go-path)
               :checker go-build)))))

    (flycheck-buttercup-def-checker-test go-test go nil
      (flycheck-buttercup-with-env
          `(("GOPATH" . ,(flycheck-buttercup-resource-filename "checkers/go")))
        (flycheck-buttercup-should-syntax-check
         "language/go/src/test/test-error_test.go" 'go-mode
         '(8 2 error "undefined: fmt" :checker go-test))))

    (flycheck-buttercup-def-checker-test go-errcheck go nil
      (flycheck-buttercup-with-env
          `(("GOPATH" . ,(flycheck-buttercup-resource-filename "language/go")))
        (flycheck-buttercup-should-syntax-check
         "language/go/src/errcheck/errcheck.go" 'go-mode
         '(7 9 warning "Ignored `error` returned from `f.Close()`"
             :checker go-errcheck)
         '(9 9 warning "Ignored `error` returned from `os.Stat(\"enoent\")`"
             :checker go-errcheck))))

    (flycheck-buttercup-def-checker-test go-unconvert go nil
      (flycheck-buttercup-with-env
          `(("GOPATH" . ,(flycheck-buttercup-resource-filename "language/go")))
        (flycheck-buttercup-should-syntax-check
         "language/go/src/unconvert/unconvert.go" 'go-mode
         '(7 17 warning "unnecessary conversion"
             :checker go-unconvert))))

    (flycheck-buttercup-def-checker-test go-staticcheck go nil
      (let ((flycheck-disabled-checkers '(go-unconvert)))
        (flycheck-buttercup-with-env
            `(("GOPATH" . ,(flycheck-buttercup-resource-filename "language/go")))
          (flycheck-buttercup-should-syntax-check
           "language/go/src/staticcheck/staticcheck1.go" 'go-mode
           '(8 6 error "unnecessary assignment to the blank identifier"
               :checker go-staticcheck :id "S1005")
           '(12 39 error "calling strings.Replace with n == 0 will return no results, did you mean -1?"
                :checker go-staticcheck :id "SA1018")
           '(16 6 error "func unused is unused"
                :checker go-staticcheck :id "U1000")))))))

;;; test-go.el ends here
