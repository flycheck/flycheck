;;; test-text.el --- Flycheck Specs: Text      -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Flycheck contributors
;; Copyright (C) 2016 Sebastian Wiesner and Flycheck contributors

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

;; Specs for text/textlint support.

;;; Code:

(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Text"
  (flycheck-buttercup-def-checker-test textlint (text markdown) nil
    (let ((flycheck-disabled-checkers '(proselint markdown-markdownlint-cli markdown-markdownlint-cli2 markdown-mdl))
          (flycheck-textlint-config "language/text/textlintrc.json"))
      (flycheck-buttercup-should-syntax-check
       "language/text/text.txt" '(text-mode markdown-mode)
       '(1 7 error "\"very\" is a weasel word and can weaken meaning"
           :id "write-good" :checker textlint))))

  (describe "reading the tool's output"
    ;; Read from output recorded earlier, so this runs whether or
    ;; not the tool is installed here

    (flycheck-buttercup-def-parse-test textlint "language/text/text.txt"
      '(1 7 error "\"very\" is a weasel word and can weaken meaning"
          :id "write-good"))

    (it "carries the fixes a fixable rule emits"
      ;; textlint's JSON rides the eslint parser, whose fix extraction
      ;; therefore works here too; this pins that compatibility, since
      ;; nothing else exercises it.  The fix's range is a pair of
      ;; absolute character offsets, wider here than the diagnostic's.
      (flycheck-buttercup-with-temp-buffer
        (insert "This is a occurence of a word.\n")
        (let* ((output "[{\"messages\": [
  {\"type\": \"lint\", \"ruleId\": \"common-misspellings\",
   \"message\": \"This is a commonly misspelled word. Correct it to occurrence\",
   \"line\": 1, \"column\": 11, \"range\": [10, 11], \"severity\": 2,
   \"fix\": {\"range\": [10, 19], \"text\": \"occurrence\"}}],
  \"filePath\": \"text.txt\"}]")
               (errs (flycheck-parse-eslint output 'textlint (current-buffer)))
               (fix (flycheck-error-fix (car errs))))
          (expect fix :not :to-be nil)
          (flycheck-apply-fix fix)
          (expect (buffer-string)
                  :to-equal "This is a occurrence of a word.\n")))))

  (describe "the textlint checker command"
    (it "appends flycheck-textlint-args before the source file"
      (flycheck-buttercup-with-temp-buffer
        (let ((flycheck-textlint-config nil)
              (flycheck-textlint-args '("--rule" "no-todo")))
          (let ((args (flycheck-checker-substituted-arguments 'textlint)))
            (expect args :to-contain "--rule")
            (expect args :to-contain "no-todo")))))))

;;; test-text.el ends here
