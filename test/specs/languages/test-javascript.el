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

  (describe "Checker tests"
    (flycheck-buttercup-def-checker-test javascript-jshint javascript syntax-error
      ;; Silence JS2 and JS3 parsers
      (let ((js2-mode-show-parse-errors nil)
            (js2-mode-show-strict-warnings nil)
            (js3-mode-show-parse-errors nil)
            (inhibit-message t)
            (flycheck-disabled-checkers
             '(javascript-eslint javascript-gjslint)))
        (flycheck-buttercup-should-syntax-check
         "language/javascript/syntax-error.js" '(js-mode js2-mode js3-mode rjsx-mode)
         '(3 1 error "Unrecoverable syntax error. (75% scanned)."
             :checker javascript-jshint :id "E041")
         '(3 25 error "Expected an identifier and instead saw ')'."
             :checker javascript-jshint :id "E030"))))

    (flycheck-buttercup-def-checker-test javascript-jshint javascript nil
      (let ((flycheck-jshintrc "jshintrc")
            (inhibit-message t)
            (flycheck-disabled-checkers
             '(javascript-eslint javascript-gjslint)))
        (flycheck-buttercup-should-syntax-check
         "language/javascript/warnings.js" '(js-mode js2-mode js3-mode rjsx-mode)
         '(4 9 warning "'foo' is defined but never used." :id "W098"
             :checker javascript-jshint))))

    (flycheck-buttercup-def-checker-test javascript-eslint javascript error
      (let ((flycheck-disabled-checkers '(javascript-jshint))
            (inhibit-message t))
        (flycheck-buttercup-should-syntax-check
         "language/javascript/syntax-error.js" flycheck-test-javascript-modes
         '(3 25 error "Parsing error: Unexpected token )" :checker javascript-eslint))))

    (flycheck-buttercup-def-checker-test javascript-eslint javascript warning
      (let ((flycheck-disabled-checkers '(javascript-jshint))
            (inhibit-message t))
        (flycheck-buttercup-should-syntax-check
         "language/javascript/warnings.js" flycheck-test-javascript-modes
         '(3 2 warning "Use the function form of 'use strict'." :id "strict"
             :checker javascript-eslint
             :end-line 5 :end-column 2)
         '(4 9 warning "'foo' is assigned a value but never used."
             :id "no-unused-vars" :checker javascript-eslint
             :end-line 4 :end-column 12))))

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
             :checker javascript-standard))))))

;;; test-javascript.el ends here
