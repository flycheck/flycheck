;;; test-scss.el --- Flycheck Specs: SCSS      -*- lexical-binding: t; -*-

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

;; Specs for SCSS support.

;;; Code:

(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language SCSS"
  (flycheck-buttercup-def-checker-test scss-lint scss nil
    (let ((flycheck-scss-lintrc "scss-lint.yml"))
      (flycheck-buttercup-should-syntax-check
       "language/scss/lint-error.scss" 'scss-mode
       '(1 1 error "Avoid using id selectors"
           :checker scss-lint :id "IdSelector")
       '(3 16 warning "Color `red` should be written in hexadecimal form as `#ff0000`"
           :checker scss-lint :id "ColorKeyword"))))

  (flycheck-buttercup-def-checker-test scss-lint scss syntax-error
    (flycheck-buttercup-should-syntax-check
     "language/scss/error.scss" 'scss-mode
     '(3 1 error "Syntax Error: Invalid CSS after \"...    c olor: red\": expected \"{\", was \";\""
         :checker scss-lint :id "Syntax")))

)

;;; test-scss.el ends here
