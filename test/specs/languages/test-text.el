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
           :id "write-good" :checker textlint)))))

;;; test-text.el ends here
