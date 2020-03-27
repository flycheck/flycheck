;;; test-typescript.el --- Flycheck Specs: TypeScript      -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Flycheck contributors
;; Copyright (C) 2016 Sebastian Wiesner and Flycheck contributors

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

;; Specs for TypeScript support.

;;; Code:

(require 'flycheck-buttercup)

(describe "Language TypeScript"
  (describe "The TSLint error parser"
    (let ((json "[{\"endPosition\":{\"character\":25,\"line\":0,\"position\":25},
  \"failure\":\"unused variable: 'invalidAlignment'\",
  \"name\":\"sample.ts\",
  \"ruleName\":\"no-unused-variable\",
  \"ruleSeverity\":\"ERROR\",
  \"startPosition\":{\"character\":9,\"line\":0,\"position\":9}},
 {\"endPosition\":{\"character\":14,\"line\":2,\"position\":76},
  \"failure\":\"missing semicolon\",
  \"name\":\"sample.ts\",
  \"ruleName\":\"semicolon\",
  \"ruleSeverity\":\"WARNING\",
  \"startPosition\":{\"character\":14,\"line\":2,\"position\":76}}]")
          (json-with-unknown-severity "no-unused-variable is deprecated. Use the tsc compiler options --noUnusedParameters and --noUnusedLocals instead.

  Could not find implementations for the following rules specified in the configuration:
      label-undefined
      no-constructor-vars
      no-duplicate-key
      no-unreachable
      use-strict
  Try upgrading TSLint and/or ensuring that you have all necessary custom rules installed.
  If TSLint was recently upgraded, you may have old rules configured which need to be cleaned up.

[{\"endPosition\":{\"character\":25,\"line\":0,\"position\":25},
 \"failure\":\"unused variable: 'invalidAlignment'\",
 \"name\":\"sample.ts\",
 \"ruleName\":\"no-unused-variable\",
 \"ruleSeverity\":\"XXX\",
 \"startPosition\":{\"character\":9,\"line\":0,\"position\":9}}]"))
      (it "parses TSLint JSON output"
        (expect (flycheck-parse-tslint json 'checker 'buffer)
                :to-be-equal-flycheck-errors
                (list
                 (flycheck-error-new-at 1 10 'error
                                        "unused variable: 'invalidAlignment'"
                                        :id "no-unused-variable"
                                        :checker 'checker
                                        :buffer 'buffer
                                        :filename "sample.ts"
                                        :end-line 1
                                        :end-column 26)
                 (flycheck-error-new-at 3 15 'warning
                                        "missing semicolon"
                                        :id "semicolon"
                                        :checker 'checker
                                        :buffer 'buffer
                                        :filename "sample.ts"
                                        :end-line 3
                                        :end-column 15))))
      (it "parses TSLint JSON output with unknown severity"
        (expect (flycheck-parse-tslint json-with-unknown-severity 'checker 'buffer)
                :to-be-equal-flycheck-errors
                (list
                 (flycheck-error-new-at 1 10 'warning
                                        "unused variable: 'invalidAlignment'"
                                        :id "no-unused-variable"
                                        :checker 'checker
                                        :buffer 'buffer
                                        :filename "sample.ts"
                                        :end-line 1
                                        :end-column 26)))))))

;;; test-typescript.el ends here
