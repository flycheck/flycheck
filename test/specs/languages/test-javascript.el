;;; test-javascript.el --- Flycheck Specs: JavaScript      -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Flycheck contributors

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

(describe "Language JavaScript"
  (describe "The ESLint error parser"
    (let ((json "[{\"filePath\":\"test/resources/language/javascript/syntax-error.js\",
  \"messages\":[{\"ruleId\":null,\"fatal\":true,\"severity\":2,
  \"source\":\"if ( /* nothing here */ ) // comment\",
  \"message\":\"Parsing error: Unexpected token )\",
  \"line\":3,\"column\":25}],
  \"errorCount\":1,\"warningCount\":0,\"fixableErrorCount\":0,\"fixableWarningCount\":0,
  \"source\":\"/** A bad if */\n\nif ( /* nothing here */ ) // comment\n\"}]")
          (json-without-errors "[{\"filePath\":\"/Users/username/Projects/elisp/flycheck/test/resources/language/javascript/jquery-3.2.1.js\",
  \"messages\":[],\"errorCount\":0,\"warningCount\":0,\"fixableErrorCount\":0,
  \"fixableWarningCount\":0}]")
          (json-with-deprecations "DeprecationWarning: [eslint] The 'ecmaFeatures' config file property is deprecated, and has no effect.\n\n[{\"filePath\":\"test/resources/language/javascript/style.js\",
  \"messages\":[{\"ruleId\":\"strict\",\"severity\":1,
  \"message\":\"Use the function form of 'use strict'.\",
  \"line\":3,\"column\":2,\"nodeType\":\"FunctionExpression\",
  \"source\":\"(function() {\",\"endLine\":5,\"endColumn\":2}],
  \"errorCount\":0,\"warningCount\":1,\"fixableErrorCount\":0,\"fixableWarningCount\":0,
  \"source\":\"/** Tab indentation */\n\n(function() {\n\tvar foo = ['Hello world'];\n}());\n\"}]"))
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
                                        :filename nil)))))))

;;; test-javascript.el ends here
