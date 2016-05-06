;;; test-typescript.el --- Flycheck Specs: TypeScript      -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2016 Sebastian Wiesner and Flycheck contributors

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

  (describe "TSLint checker"

    (defconst flycheck/typescript-lint-json
      "[{\"endPosition\":{\"character\":25,\"line\":0,\"position\":25},
  \"failure\":\"unused variable: 'invalidAlignment'\",
  \"name\":\"sample.ts\",
  \"ruleName\":\"no-unused-variable\",
  \"startPosition\":{\"character\":9,\"line\":0,\"position\":9}},
 {\"endPosition\":{\"character\":14,\"line\":2,\"position\":76},
  \"failure\":\"missing semicolon\",
  \"name\":\"sample.ts\",
  \"ruleName\":\"semicolon\",
  \"startPosition\":{\"character\":14,\"line\":2,\"position\":76}}]"
      "Example TSLint JSON output")

    (it "should parse example TSLint JSON output"
      (expect (flycheck-parse-tslint flycheck/typescript-lint-json
                                     'checker 'buffer)
              :to-be-equal-flycheck-errors
              (list
               (flycheck-error-new-at 1 10 'warning
                                      "unused variable: 'invalidAlignment'"
                                      :id "no-unused-variable"
                                      :checker 'checker
                                      :buffer 'buffer
                                      :filename "sample.ts")
               (flycheck-error-new-at 3 15 'warning
                                      "missing semicolon"
                                      :id "semicolon"
                                      :checker 'checker
                                      :buffer 'buffer
                                      :filename "sample.ts"))))))

;;; test-typescript.el ends here
