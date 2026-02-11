;;; test-tex.el --- Flycheck Specs: TeX/LaTeX      -*- lexical-binding: t; -*-

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

;; Specs for TeX/LaTeX support.

;;; Code:

(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language TeX"
  (flycheck-buttercup-def-checker-test tex-chktex (tex latex) nil
    (flycheck-buttercup-should-syntax-check
     "language/tex.tex" 'latex-mode
     '(5 29 warning "Intersentence spacing (`\\@') should perhaps be used."
         :id "13" :checker tex-chktex)))

  (flycheck-buttercup-def-checker-test tex-lacheck (tex latex) nil
    (let ((flycheck-disabled-checkers '(tex-chktex)))
      (flycheck-buttercup-should-syntax-check
       "language/tex.tex" 'latex-mode
       '(5 nil warning "missing `\\@' before `.' in \"GNU.\""
           :checker tex-lacheck)
       '(7 nil warning "possible unwanted space at \"{\""
           :checker tex-lacheck)))))

;;; test-tex.el ends here
