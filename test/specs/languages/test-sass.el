;;; test-sass.el --- Flycheck Specs: Sass      -*- lexical-binding: t; -*-

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

;; Specs for Sass support.

;;; Code:

(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Sass"
  (flycheck-buttercup-def-checker-test sass sass nil
    (let ((flycheck-disabled-checkers '(sass-stylelint sass/scss-sass-lint)))
      (flycheck-buttercup-should-syntax-check
       "language/sass/error.sass" 'sass-mode
       '(5 nil error "Inconsistent indentation: 3 spaces were used for indentation, but the rest of the document was indented using 2 spaces."
           :checker sass))))

  (flycheck-buttercup-def-checker-test sass sass warning
    (let ((flycheck-disabled-checkers '(sass-stylelint sass/scss-sass-lint)))
      (flycheck-buttercup-should-syntax-check
       "language/sass/warning.sass" 'sass-mode
       '(2 nil warning "this is deprecated" :checker sass)))))

;;; test-sass.el ends here
