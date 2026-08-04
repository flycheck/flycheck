;;; test-javascript.el --- Flycheck Specs: JavaScript      -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018 Flycheck contributors

;; Author: Saša Jovanić <info@simplify.ba>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Specs for JavaScript support.

;;; Code:

(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language JavaScript"
  (describe "The ESLint error parser"
    (let ((json "[{\"filePath\":\"test/resources/language/javascript/syntax-error.js\",
  \"messages\":[{\"ruleId\":null,\"fatal\":true,\"severity\":2,
  \"source\":\"if ( /* nothing here */ ) // comment\",
  \"message\":\"Parsing error: Unexpected token )\",
  \"line\":3,\"column\":25}],
  \"errorCount\":1,\"warningCount\":0,\"fixableErrorCount\":0,\"fixableWarningCount\":0,
  \"source\":\"/** A bad if */if ( /* nothing here */ ) // comment\"}]")
          (json-without-errors "[{\"filePath\":\"/Users/username/Projects/elisp/flycheck/test/resources/language/javascript/jquery-3.2.1.js\",
  \"messages\":[],\"errorCount\":0,\"warningCount\":0,\"fixableErrorCount\":0,
  \"fixableWarningCount\":0}]")
          (json-with-deprecations "DeprecationWarning: [eslint] The 'ecmaFeatures' config file property is deprecated, and has no effect.\n\n[{\"filePath\":\"test/resources/language/javascript/style.js\",
  \"messages\":[{\"ruleId\":\"strict\",\"severity\":1,
  \"message\":\"Use the function form of 'use strict'.\",
  \"line\":3,\"column\":2,\"nodeType\":\"FunctionExpression\",
  \"source\":\"(function() {\",\"endLine\":5,\"endColumn\":2}],
  \"errorCount\":0,\"warningCount\":1,\"fixableErrorCount\":0,\"fixableWarningCount\":0,
  \"source\":\"/** Tab indentation */(function() {var foo = ['Hello world'];}());\"}]"))
      (it "parses ESLint JSON output with errors"
        (expect (flycheck-parse-eslint json 'checker nil)
                :to-be-equal-flycheck-errors
                (list
                 (flycheck-error-new-at 3 25 'error
                                        "Parsing error: Unexpected token )"
                                        :id nil
                                        :checker 'checker
                                        :buffer nil
                                        :filename nil))))
      (it "parses ESLint JSON output without errors"
        (expect (flycheck-parse-eslint json-without-errors 'checker 'buffer)
                :to-be-equal-flycheck-errors ()))
      (it "parses ESLint JSON output with deprecation warning"
        (expect (flycheck-parse-eslint json-with-deprecations 'checker nil)
                :to-be-equal-flycheck-errors
                (list
                 (flycheck-error-new-at 3 2 'warning
                                        "Use the function form of 'use strict'."
                                        :id "strict"
                                        :checker 'checker
                                        :buffer nil
                                        :filename nil
                                        :end-line 5
                                        :end-column 2))))))

  (describe "Fatal-failure handling"
    (it "disables the checker on a fatal eslint exit"
      ;; Exit status 2 is eslint's fatal-error status, used for missing
      ;; and broken configurations alike, in every eslint version
      (expect (flycheck--eslint-handle-suspicious
               'javascript-eslint 2
               "ESLint couldn't find an eslint.config.(js|mjs|cjs) file.
Some more explanation here.")
              :to-equal
              '(disable
                . "ESLint couldn't find an eslint.config.(js|mjs|cjs) file.")))

    (it "stays suspicious on unparsable lint results"
      ;; Exit status 1 means eslint found lint problems; reaching the
      ;; suspicious handler then means we failed to parse them
      (expect (flycheck--eslint-handle-suspicious
               'javascript-eslint 1 "[{\"unexpected\": \"format\"}]")
              :to-be 'suspicious)))

  (describe "flycheck-eslint-config-exists-p"
    (it "is non-nil when eslint exits zero"
      (spy-on 'flycheck-call-checker-process :and-return-value 0)
      (expect (flycheck-eslint-config-exists-p) :to-be-truthy))

    (it "is nil when eslint exits non-zero"
      (spy-on 'flycheck-call-checker-process :and-return-value 1)
      (expect (flycheck-eslint-config-exists-p) :to-be nil))

    (it "is nil, not an error, when eslint cannot be found"
      ;; Regression for #2232: `flycheck-call-checker-process' returns nil when
      ;; the executable is missing, and that nil must not reach `zerop'.
      (spy-on 'flycheck-call-checker-process :and-return-value nil)
      (expect (flycheck-eslint-config-exists-p) :to-be nil)))

  (describe "flycheck-eslint--find-working-directory"
    (defun test-eslint/root-with (config-file)
      "Return the detected root for a buffer under a dir holding CONFIG-FILE."
      (let* ((root (make-temp-file "flycheck-eslint" 'dir))
             (srcdir (expand-file-name "src/" root))
             (src (expand-file-name "app.js" srcdir)))
        (unwind-protect
            (progn
              (make-directory srcdir)
              (write-region "" nil (expand-file-name config-file root))
              (write-region "" nil src)
              (with-temp-buffer
                (setq buffer-file-name src)
                (flycheck-eslint--find-working-directory nil)))
          (delete-directory root 'recursive))))

    (it "detects a project root from a flat config file"
      (dolist (name '("eslint.config.js" "eslint.config.mjs"
                      "eslint.config.cjs" "eslint.config.ts"))
        (expect (test-eslint/root-with name) :not :to-be nil)))

    (it "still detects a project root from a legacy .eslintrc"
      (dolist (name '(".eslintrc" ".eslintrc.js" ".eslintrc.json"
                      ".eslintrc.yml"))
        (expect (test-eslint/root-with name) :not :to-be nil)))

    (it "does not treat an unrelated dotfile as a config"
      (expect (test-eslint/root-with ".prettierrc") :to-be nil)))

  (describe "Checker tests"
    (flycheck-buttercup-def-checker-test javascript-eslint javascript error
      (let ((inhibit-message t))
        (flycheck-buttercup-should-syntax-check
         "language/javascript/syntax-error.js" flycheck-test-javascript-modes
         '(3 25 error "Parsing error: Unexpected token )" :checker javascript-eslint))))

    (flycheck-buttercup-def-checker-test javascript-eslint javascript warning
      (let ((inhibit-message t))
        (flycheck-buttercup-should-syntax-check
         "language/javascript/warnings.js" flycheck-test-javascript-modes
         '(3 2 warning "Use the function form of 'use strict'." :id "strict"
             :checker javascript-eslint
             :end-line 5 :end-column 2)
         '(4 9 warning "'foo' is assigned a value but never used."
             :id "no-unused-vars" :checker javascript-eslint
             :end-line 4 :end-column 12))))

    (flycheck-buttercup-def-checker-test javascript-oxlint javascript nil
      (let ((flycheck-checker 'javascript-oxlint)
            (inhibit-message t))
        (flycheck-buttercup-should-syntax-check
         "language/javascript/warnings.js" flycheck-test-javascript-modes
         '(4 9 warning "Variable 'foo' is declared but never used. Unused variables should start with a '_'."
             :id "eslint(no-unused-vars)" :checker javascript-oxlint))))

    (flycheck-buttercup-def-checker-test javascript-standard javascript error
      (let ((flycheck-checker 'javascript-standard)
            (inhibit-message t))
        (flycheck-buttercup-should-syntax-check
         "language/javascript/style.js" flycheck-test-javascript-modes
         '(3 10 error "Missing space before function parentheses."
             :checker javascript-standard)
         '(4 1 error "Unexpected tab character."
             :checker javascript-standard)
         '(4 1 error "Expected indentation of 2 spaces but found 1 tab."
             :checker javascript-standard)
         '(4 2 error "Unexpected var, use let or const instead."
             :checker javascript-standard)
         '(4 6 error "'foo' is assigned a value but never used."
             :checker javascript-standard)
         '(4 13 error "Strings must use singlequote."
             :checker javascript-standard)
         '(4 27 error "Extra semicolon."
             :checker javascript-standard)
         '(5 5 error "Extra semicolon."
             :checker javascript-standard))))

    (flycheck-buttercup-def-checker-test javascript-standard javascript semistandard
      (let ((flycheck-checker 'javascript-standard)
            (flycheck-javascript-standard-executable "semistandard")
            (inhibit-message t))
        (flycheck-buttercup-should-syntax-check
         "language/javascript/style.js" flycheck-test-javascript-modes
         '(3 10 error "Missing space before function parentheses."
             :checker javascript-standard)
         '(4 1 error "Unexpected tab character."
             :checker javascript-standard)
         '(4 1 error "Expected indentation of 2 spaces but found 1 tab."
             :checker javascript-standard)
         '(4 2 error "Unexpected var, use let or const instead."
             :checker javascript-standard)
         '(4 6 error "'foo' is assigned a value but never used."
             :checker javascript-standard)
         '(4 13 error "Strings must use singlequote."
             :checker javascript-standard)))))

  (describe "the javascript-oxlint checker command"
    (it "passes the deny and allow rules as repeated options"
      (let ((flycheck-javascript-oxlint-deny '("pedantic" "no-debugger"))
            (flycheck-javascript-oxlint-allow '("no-console")))
        ;; The trailing argument is the temporary copy of the buffer
        (expect (butlast (flycheck-checker-substituted-arguments 'javascript-oxlint))
                :to-equal '("--format" "checkstyle"
                            "--deny" "pedantic" "--deny" "no-debugger"
                            "--allow" "no-console"))))

    (it "passes flycheck-javascript-oxlint-config via --config"
      (let ((flycheck-javascript-oxlint-config ".oxlintrc.json"))
        (spy-on 'flycheck-locate-config-file :and-return-value "/c/.oxlintrc.json")
        (expect (flycheck-checker-substituted-arguments 'javascript-oxlint)
                :to-contain "--config")))

    (it "threads flycheck-javascript-oxlint-args into oxlint"
      (let ((flycheck-javascript-oxlint-args '("--react-plugin")))
        (expect (flycheck-checker-substituted-arguments 'javascript-oxlint)
                :to-contain "--react-plugin"))))

  (describe "the javascript-standard checker command"
    (it "threads flycheck-javascript-standard-args into standard"
      (let ((flycheck-javascript-standard-args '("--env" "mocha")))
        (expect (flycheck-checker-substituted-arguments 'javascript-standard)
                :to-equal '("--stdin" "--env" "mocha")))))

  (describe "reading the tool's output"
    ;; Read from output recorded earlier, so this runs whether or
    ;; not the tool is installed here

    (flycheck-buttercup-def-parse-test javascript-oxlint "language/javascript/warnings.js"
      '(4 9 warning "Variable 'foo' is declared but never used. Unused variables should start with a '_'." :id "eslint(no-unused-vars)"))
    (flycheck-buttercup-def-parse-test javascript-standard "language/javascript/style.js"
      '(3 10 error "Missing space before function parentheses. (space-before-function-paren)")
      '(4 1 error "Unexpected tab character. (no-tabs)")
      '(4 1 error "Expected indentation of 2 spaces but found 1 tab. (indent)")
      '(4 2 error "Unexpected var, use let or const instead. (no-var) (warning)")
      '(4 6 error "'foo' is assigned a value but never used. (no-unused-vars)")
      '(4 13 error "Strings must use singlequote. (quotes)")
      '(4 27 error "Extra semicolon. (semi)")
      '(5 5 error "Extra semicolon. (semi)"))))

;;; test-javascript.el ends here
